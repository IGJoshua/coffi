(ns coffi.ffi
  "Functions for managing native allocations and resource scopes, creating handles
  to native functions, serializing and deserializing Clojure data to native
  structures, and loading native libraries."
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [coffi.mem :as mem]
   [insn.core :as insn])
  (:import
   (clojure.lang
    IDeref IFn IMeta IObj IReference)
   (coffi.ffi Loader)
   (java.lang.invoke
    MethodHandle
    MethodHandles
    MethodType)
   (jdk.incubator.foreign
    Addressable
    CLinker
    FunctionDescriptor
    MemoryLayout
    SegmentAllocator)))

;;; FFI Code loading and function access

(defn load-system-library
  "Loads the library named `libname` from the system's load path."
  [libname]
  (Loader/loadSystemLibrary (name libname)))

(defn load-library
  "Loads the library at `path`."
  [path]
  (Loader/loadLibrary (.getAbsolutePath (io/file path))))

(defn find-symbol
  "Gets the [[MemoryAddress]] of a symbol from the loaded libraries."
  [sym]
  (let [sym (name sym)]
    (Loader/findSymbol sym)))

(defn- method-type
  "Gets the [[MethodType]] for a set of `args` and `ret` types."
  ([args] (method-type args ::mem/void))
  ([args ret]
   (MethodType/methodType
    ^Class (mem/java-layout ret)
    ^"[Ljava.lang.Class;" (into-array Class (map mem/java-layout args)))))

(defn- function-descriptor
  "Gets the [[FunctionDescriptor]] for a set of `args` and `ret` types."
  ([args] (function-descriptor args ::mem/void))
  ([args ret]
   (let [args-arr (into-array MemoryLayout (map mem/c-layout args))]
     (if-not (identical? ret ::mem/void)
       (FunctionDescriptor/of
        (mem/c-layout ret)
        args-arr)
       (FunctionDescriptor/ofVoid
        args-arr)))))

(defn- downcall-handle
  "Gets the [[MethodHandle]] for the function at the `address`."
  [address method-type function-descriptor]
  (.downcallHandle (CLinker/getInstance) address method-type function-descriptor))

(def ^:private load-instructions
  "Mapping from primitive types to the instruction used to load them onto the stack."
  {::mem/byte :bload
   ::mem/short :sload
   ::mem/int :iload
   ::mem/long :lload
   ::mem/long-long :lload
   ::mem/char :cload
   ::mem/float :fload
   ::mem/double :dload
   ::mem/pointer :aload})

(def ^:private prim-classes
  "Mapping from primitive types to their box classes."
  {::mem/byte Byte
   ::mem/short Short
   ::mem/int Integer
   ::mem/long Long
   ::mem/long-long Long
   ::mem/char Character
   ::mem/float Float
   ::mem/double Double})

(defn- to-object-asm
  "Constructs a bytecode sequence to box a primitive on the top of the stack.

  If the `type` is not primitive, then no change will occur. If it is void, a
  null reference will be pushed to the stack."
  [type]
  (cond
    (identical? ::mem/void type) [:ldc nil]
    (identical? ::mem/pointer (mem/primitive-type type)) []
    :else
    (let [prim-type (some-> type mem/primitive-type)]
      (if-some [prim  (some-> prim-type name keyword)]
       ;; Box primitive
       [:invokestatic (prim-classes prim-type) "valueOf" [prim (prim-classes prim-type)]]
       ;; Return object without change
       []))))

(defn- insn-layout
  "Gets the type keyword or class for referring to the type in bytecode."
  [type]
  (or (when-some [prim (mem/primitive-type type)]
        (when (not= prim ::mem/pointer)
          (keyword (name prim))))
      (mem/java-layout type)))

(def ^:private unbox-fn-for-type
  "Map from type name to the name of its unboxing function."
  {::mem/byte "byteValue"
   ::mem/short "shortValue"
   ::mem/int "intValue"
   ::mem/long "longValue"
   ::mem/long-long "longValue"
   ::mem/char "charValue"
   ::mem/float "floatValue"
   ::mem/double "doubleValue"})

(defn- to-prim-asm
  "Constructs a bytecode sequence to unbox a primitive type on top of the stack.

  If the `type` is not primitive, then no change will occur. If it is void, it
  will be popped."
  [type]
  (cond
    (identical? ::mem/void type) [:pop]
    (identical? ::mem/pointer (mem/primitive-type type)) []
    :else
    (let [prim-type (some-> type mem/primitive-type)]
      (if-some [prim (some-> prim-type name keyword)]
        [[:checkcast (prim-classes prim-type)]
         [:invokevirtual (prim-classes prim-type) (unbox-fn-for-type prim-type) [prim]]]
        []))))

(defn- downcall-class
  "Class definition for an implementation of [[IFn]] which calls a closed over
  method handle without reflection, unboxing primitives when needed."
  [args ret]
  {:flags #{:public :final}
    :super clojure.lang.AFunction
    :fields [{:name "downcall_handle"
              :type MethodHandle
              :flags #{:final}}]
    :methods [{:name :init
               :flags #{:public}
               :desc [MethodHandle :void]
               :emit [[:aload 0]
                      [:dup]
                      [:invokespecial :super :init [:void]]
                      [:aload 1]
                      [:putfield :this "downcall_handle" MethodHandle]
                      [:return]]}
              {:name :invoke
               :flags #{:public}
               :desc (repeat (cond-> (inc (count args))
                               (not (mem/primitive-type ret)) inc)
                             Object)
               :emit [[:aload 0]
                      [:getfield :this "downcall_handle" MethodHandle]
                      (when-not (mem/primitive-type ret)
                        [[:aload 1]
                         [:checkcast SegmentAllocator]])
                      (map-indexed
                       (fn [idx arg]
                         [[:aload (cond-> (inc idx)
                                    (not (mem/primitive-type ret)) inc)]
                          (to-prim-asm arg)])
                       args)
                      [:invokevirtual MethodHandle "invokeExact"
                       (cond->>
                           (conj (mapv insn-layout args)
                                 (insn-layout ret))
                         (not (mem/primitive-type ret)) (cons SegmentAllocator))]
                      (to-object-asm ret)
                      [:areturn]]}]})

(defn- downcall-fn
  "Creates a function to call `handle` without reflection."
  [handle args ret]
  (insn/new-instance (downcall-class args ret) ^MethodHandle handle))

(defn- ensure-address
  "Gets the address if the argument is [[Addressable]], otherwise
  calls [[find-symbol]] on it."
  [symbol-or-addr]
  (if (instance? Addressable symbol-or-addr)
    (mem/address-of symbol-or-addr)
    (find-symbol symbol-or-addr)))

(defn make-downcall
  "Constructs a downcall function reference to `symbol-or-addr` with the given `args` and `ret` types.

  The function returned takes only arguments whose types match exactly
  the [[java-layout]] for that type, and returns an argument with exactly
  the [[java-layout]] of the `ret` type. This function will perform no
  serialization or deserialization of arguments or the return type.

  If the `ret` type is non-primitive, then the returned function will take a
  first argument of a [[SegmentAllocator]]."
  [symbol-or-addr args ret]
  (-> symbol-or-addr
      ensure-address
      (downcall-handle
       (method-type args ret)
       (function-descriptor args ret))
      (downcall-fn args ret)))

(defn make-varargs-factory
  "Returns a function for constructing downcalls with additional types for arguments.

  The `required-args` are the types of the first arguments passed to the
  downcall handle, and the values passed to the returned function are only the
  varargs types.

  The returned function is memoized, so that only one downcall function will be
  generated per combination of argument types.

  See [[make-downcall]]."
  [symbol required-args ret]
  (memoize
   (fn [& types]
     (let [args (concat required-args types)]
       (make-downcall symbol args ret)))))

;; TODO(Joshua): Optimize this to not serialize things if possible
(defn make-serde-wrapper
  "Constructs a wrapper function for the `downcall` which serializes the arguments
  and deserializes the return value."
  [downcall arg-types ret-type]
  (if (mem/primitive-type ret-type)
    (fn native-fn [& args]
      (with-open [scope (mem/stack-scope)]
        (mem/deserialize
         (apply downcall (map #(mem/serialize %1 %2 scope) args arg-types))
         ret-type)))
    (fn native-fn [& args]
      (with-open [scope (mem/stack-scope)]
        (mem/deserialize
         (apply downcall (mem/scope-allocator scope)
                (map #(mem/serialize %1 %2 scope) args arg-types))
         ret-type)))))

(defn make-serde-varargs-wrapper
  "Constructs a wrapper function for the `varargs-factory` which produces
  functions that serialize the arguments and deserialize the return value."
  [varargs-factory required-args ret-type]
  (memoize
   (fn [& types]
     (let [args-types (concat required-args types)]
       (make-serde-wrapper
        (apply varargs-factory types)
        args-types
        ret-type)))))

(defn cfn
  "Constructs a Clojure function to call the native function referenced by `symbol`.

  The function returned will serialize any passed arguments into the `args`
  types, and deserialize the return to the `ret` type."
  [symbol args ret]
  (-> symbol
      ensure-address
      (make-downcall args ret)
      (cond->
        (every? #(= % (mem/primitive-type %))
                (cons ret args))
        (make-serde-wrapper args ret))))

(defn vacfn-factory
  "Constructs a varargs factory to call the native function referenced by `symbol`.

  The function returned takes any number of type arguments and returns a
  specialized Clojure function for calling the native function with those
  arguments."
  [symbol required-args ret]
  (-> symbol
      ensure-address
      (make-varargs-factory required-args ret)
      (make-serde-varargs-wrapper required-args ret)))

;;; Function types

(def ^:private return-for-type
  "Map from type name to the return instruction for that type."
  {::mem/byte :breturn
   ::mem/short :sreturn
   ::mem/int :ireturn
   ::mem/long :lreturn
   ::mem/long-long :lreturn
   ::mem/char :creturn
   ::mem/float :freturn
   ::mem/double :dreturn
   ::mem/void :return})

(defn- upcall-class
  "Constructs a class definition for a class with a single method, `upcall`, which
  boxes any primitives passed to it and calls a closed over [[IFn]]."
  [arg-types ret-type]
  {:flags #{:public :final}
   :fields [{:name "upcall_ifn"
             :type IFn
             :flags #{:final}}]
   :methods [{:name :init
              :flags #{:public}
              :desc [IFn :void]
              :emit [[:aload 0]
                     [:dup]
                     [:invokespecial :super :init [:void]]
                     [:aload 1]
                     [:putfield :this "upcall_ifn" IFn]
                     [:return]]}
             {:name :upcall
              :flags #{:public}
              :desc (conj (mapv insn-layout arg-types)
                          (insn-layout ret-type))
              :emit [[:aload 0]
                     [:getfield :this "upcall_ifn" IFn]
                     (map-indexed
                      (fn [idx arg]
                        [[(load-instructions (mem/primitive-type arg) :aload) (inc idx)]
                         (to-object-asm arg)])
                      arg-types)
                     [:invokeinterface IFn "invoke" (repeat (inc (count arg-types)) Object)]
                     (to-prim-asm ret-type)
                     [(return-for-type ret-type :areturn)]]}]})

(defn- upcall
  "Constructs an instance of [[upcall-class]], closing over `f`."
  [f arg-types ret-type]
  (insn/new-instance (upcall-class arg-types ret-type) ^IFn f))

(defn- upcall-handle
  "Constructs a method handle for invoking `f`, a function of `arg-count` args."
  [f arg-types ret-type]
  (.bind
   (MethodHandles/lookup)
   (upcall f arg-types ret-type)
   "upcall"
   (method-type arg-types ret-type)))

(defmethod mem/primitive-type ::fn
  [_type]
  ::mem/pointer)

(defn- upcall-serde-wrapper
  "Creates a function that wraps `f` which deserializes the arguments and
  serializes the return type in the [[global-scope]]."
  [f arg-types ret-type]
  (fn [& args]
    (mem/serialize
     (apply f (map mem/deserialize args arg-types))
     ret-type
     (mem/global-scope))))

(defmethod mem/serialize* ::fn
  [f [_fn arg-types ret-type & {:keys [raw-fn?]}] scope]
  (.upcallStub
   (CLinker/getInstance)
   (cond-> f
     (not raw-fn?) (upcall-serde-wrapper arg-types ret-type)
     :always (upcall-handle arg-types ret-type))
   (function-descriptor arg-types ret-type)
   scope))

(defmethod mem/deserialize* ::fn
  [addr [_fn arg-types ret-type & {:keys [raw-fn?]}]]
  (when-not (mem/null? addr)
    (vary-meta
      (-> addr
          (downcall-handle
           (method-type arg-types ret-type)
           (function-descriptor arg-types ret-type))
          (downcall-fn arg-types ret-type)
          (cond->
              (not raw-fn?) (make-serde-wrapper arg-types ret-type)))
      assoc ::address addr)))

;;; Static memory access

(defn const
  "Gets the value of a constant stored in `symbol-or-addr`."
  [symbol-or-addr type]
  (mem/deserialize (ensure-address symbol-or-addr) [::mem/pointer type]))

(deftype StaticVariable [addr type meta]
  Addressable
  (address [_]
    addr)
  IDeref
  (deref [_]
    (mem/deserialize addr [::mem/pointer type]))

  IObj
  (withMeta [_ meta-map]
    (StaticVariable. addr type (atom meta-map)))
  IMeta
  (meta [_]
    @meta)
  IReference
  (resetMeta [_ meta-map]
    (reset! meta meta-map))
  (alterMeta [_ f args]
    (apply swap! meta f args)))

(defn freset!
  "Sets the value of `static-var` to `newval`, running it through [[serialize]]."
  [^StaticVariable static-var newval]
  (mem/serialize-into
   newval (.-type static-var)
   (mem/slice-global (.-addr static-var) (mem/size-of (.-type static-var)))
   (mem/global-scope))
  newval)

(defn fswap!
  "Non-atomically runs the function `f` over the value stored in `static-var`.

  The value is deserialized before passing it to `f`, and serialized before
  putting the value into `static-var`."
  [static-var f & args]
  (freset! static-var (apply f @static-var args)))

(defn static-variable
  "Constructs a reference to a mutable value stored in `symbol-or-addr`.

  The returned value can be dereferenced, and has metadata, and the address of
  the value can be queried with [[address-of]].

  See [[freset!]], [[fswap!]]."
  [symbol-or-addr type]
  (StaticVariable. (ensure-address symbol-or-addr) type (atom nil)))

(s/def :coffi.ffi.symbolspec/symbol string?)
(s/def :coffi.ffi.symbolspec/type keyword?)
(s/def ::symbolspec
  (s/keys :req-un [:coffi.ffi.symbolspec/type :coffi.ffi.symbolspec/symbol]))

(defmulti reify-symbolspec
  "Takes a spec for a symbol reference and returns a live value for that type."
  :type)
(s/fdef reify-symbolspec
  :args (s/cat :spec ::symbolspec))

(defmethod reify-symbolspec :function
  [spec]
  (cond->
      (make-downcall (:symbol spec)
                     (:function/args spec)
                     (:function/ret spec))
    (not (:function/raw-fn? spec))
    (make-serde-wrapper
     (:function/args spec)
     (:function/ret spec))))

(defmethod reify-symbolspec :varargs-factory
  [spec]
  (cond->
      (make-varargs-factory (:symbol spec)
                            (:function/args spec)
                            (:function/ret spec))
    (not (:function/raw-fn? spec))
    (make-serde-varargs-wrapper
     (:function/args spec)
     (:function/ret spec))))

(defmethod reify-symbolspec :const
  [spec]
  (const (:symbol spec)
         (:const/type spec)))

(defmethod reify-symbolspec :static-var
  [spec]
  (static-variable (:symbol spec)
                   (:static-var/type spec)))

(s/def ::libspec
  (s/map-of keyword? ::symbolspec))

(defn reify-libspec
  "Loads all the symbols specified in the `libspec`.

  The value of each key of the passed map is transformed as
  by [[reify-symbolspec]]."
  [libspec]
  (reduce-kv
   (fn [m k v]
     (assoc m k
            (reify-symbolspec v)))
   {}
   libspec))
(s/fdef reify-libspec
  :args (s/cat :libspec ::libspec)
  :ret (s/map-of keyword? any?))

(s/def ::defcfn-args
  (s/and
   (s/cat :name simple-symbol?
          :doc (s/? string?)
          :attr-map (s/? map?)
          :symbol (s/nonconforming
                   (s/or :string string?
                         :symbol simple-symbol?))
          :native-arglist (s/coll-of ::mem/type :kind vector?)
          :return-type ::mem/type
          :wrapper (s/?
                    (s/cat
                     :native-fn simple-symbol?
                     :fn-tail (let [fn-tail (s/cat :arglist (s/coll-of simple-symbol? :kind vector?)
                                                   :body (s/* any?))]
                                (s/alt
                                 :single-arity fn-tail
                                 :multi-arity (s/+ (s/spec fn-tail)))))))
   #(if (:wrapper %)
      (not= (:name %) (-> % :wrapper :native-fn))
      true)))

(defmacro defcfn
  "Defines a Clojure function which maps to a native function.

  `name` is the symbol naming the resulting var.
  `symbol` is a symbol or string naming the library symbol to link against.
  `arg-types` is a vector of qualified keywords representing the argument types.
  `ret-type` is a single qualified keyword representing the return type.
  `fn-tail` is the body of the function (potentially with multiple arities)
  which wraps the native one. Inside the function, `native-fn` is bound to a
  function that will serialize its arguments, call the native function, and
  deserialize its return type. If any body is present, you must call this
  function in order to call the native code.

  If no `fn-tail` is provided, then the resulting function will simply serialize
  the arguments according to `arg-types`, call the native function, and
  deserialize the return value.

  The number of args in the `fn-tail` need not match the number of `arg-types`
  for the native function. It need only call the native wrapper function with
  the correct arguments.

  See [[serialize]], [[deserialize]], [[make-downcall]]."
  {:arglists '([name docstring? attr-map? symbol arg-types ret-type]
               [name docstring? attr-map? symbol arg-types ret-type native-fn & fn-tail])
   :style/indent [:defn]}
  [& args]
  (let [args (s/conform ::defcfn-args args)
        address (gensym "symbol")
        native-sym (gensym "native")
        [arity fn-tail] (-> args :wrapper :fn-tail)
        fn-tail (case arity
                  :single-arity (cons (:arglist fn-tail) (:body fn-tail))
                  :multi-arity (map #(cons (:arglist %) (:body %)) fn-tail)
                  nil)
        arglists (map first (case arity
                              :single-arity [fn-tail]
                              :multi-arity fn-tail
                              nil))]
    `(let [~address (find-symbol ~(name (:symbol args)))
           ~native-sym (cfn ~address ~(:native-arglist args) ~(:return-type args))
           fun# ~(if (:wrapper args)
                   `(fn ~(:name args)
                      ~@fn-tail)
                   native-sym)]
       (def
         ~(with-meta (:name args)
            (merge (update (meta (:name args)) :arglists
                           (fn [old-list]
                             (list
                              'quote
                              (or old-list
                                  (seq arglists)
                                  (list
                                   (mapv (fn [type]
                                           (-> (cond-> type
                                                 (vector? type) first)
                                               name
                                               symbol))
                                         (:native-arglist args)))))))
                   (assoc (:attr-map args)
                          ::address address)))
         ~@(when-let [doc (:doc args)]
             (list doc))
         fun#))))
(s/fdef defcfn
  :args ::defcfn-args)
