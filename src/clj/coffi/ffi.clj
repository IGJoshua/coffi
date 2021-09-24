(ns coffi.ffi
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
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
    MemoryAccess
    MemoryAddress
    MemoryLayout
    MemorySegment
    ResourceScope
    SegmentAllocator)))

(defn stack-scope
  "Constructs a new scope for use only in this thread.

  The memory allocated within this scope is cheap to allocate, like a native
  stack."
  ^ResourceScope []
  (ResourceScope/newConfinedScope))

(defn shared-scope
  "Constructs a new shared scope.

  This scope can be shared across threads and memory allocated in it will only
  be cleaned up once every thread accessing the scope closes it."
  ^ResourceScope []
  (ResourceScope/newSharedScope))

(defn connected-scope
  "Constructs a new scope to reclaim all connected resources at once.

  The scope may be shared across threads, and all resources created with it will
  be cleaned up at the same time, when all references have been collected.

  This type of scope cannot be closed, and therefore should not be created in
  a [[with-open]] clause."
  ^ResourceScope []
  (ResourceScope/newImplicitScope))

(defn global-scope
  "Constructs the global scope, which will never reclaim its resources.

  This scope may be shared across threads, but is intended mainly in cases where
  memory is allocated with [[alloc]] but is either never freed or whose
  management is relinquished to a native library, such as when returned from a
  callback."
  ^ResourceScope []
  (ResourceScope/globalScope))

(defn scope-allocator
  "Constructs a segment allocator from the given `scope`.

  This is primarily used when working with unwrapped downcall functions. When a
  downcall function returns a non-primitive type, it must be provided with an
  allocator."
  ^SegmentAllocator [^ResourceScope scope]
  (SegmentAllocator/ofScope scope))

(defn alloc
  "Allocates `size` bytes.

  If a `scope` is provided, the allocation will be reclaimed when it is closed."
  ([size] (alloc size (connected-scope)))
  ([size scope] (MemorySegment/allocateNative (long size) ^ResourceScope scope)))

(defn alloc-with
  "Allocates `size` bytes using the `allocator`."
  ([allocator size]
   (.allocate ^SegmentAllocator allocator (long size)))
  ([allocator size alignment]
   (.allocate ^SegmentAllocator allocator (long size) (long alignment))))

(defmacro with-acquired
  "Acquires a `scope` to ensure it will not be released until the `body` completes.

  This is only necessary to do on shared scopes, however if you are operating on
  an arbitrary passed scope, it is best practice to wrap code that interacts
  with it wrapped in this."
  [scope & body]
  `(let [scope# ~scope
         handle# (.acquire ^ResourceScope scope#)]
     (try ~@body
          (finally (.release ^ResourceScope scope# handle#)))))

(defn address-of
  "Gets the address of a given segment.

  This value can be used as an argument to functions which take a pointer."
  [addressable]
  (.address ^Addressable addressable))

(defn null?
  "Checks if a memory address is null."
  [addr]
  (.equals (MemoryAddress/NULL) addr))

(defn slice-global
  "Gets a slice of the global address space.

  Because this fetches from the global segment, it has no associated scope, and
  therefore the reference created here cannot prevent the value from being
  freed. Be careful to ensure that you are not retaining an object incorrectly."
  [address size]
  (.asSlice (MemorySegment/globalNativeSegment)
            ^MemoryAddress address (long size)))

(defn slice
  "Get a slice over the `segment` with the given `offset`."
  ([segment offset]
   (.asSlice ^MemorySegment segment (long offset)))
  ([segment offset size]
   (.asSlice ^MemorySegment segment (long offset) (long size))))

(defn slice-into
  "Get a slice into the `segment` starting at the `address`."
  ([address segment]
   (.asSlice ^MemorySegment segment ^MemoryAddress address))
  ([address segment size]
   (.asSlice ^MemorySegment segment ^MemoryAddress address (long size))))

(defn with-offset
  "Get a new address `offset` from the old `address`."
  [address offset]
  (.addOffset ^MemoryAddress address (long offset)))

(defn as-segment
  "Dereferences an `address` into a memory segment associated with the `scope`.

  If `cleanup` is provided, it is a 0-arity function run when the scope is
  closed. This can be used to register a free method for the memory, or do other
  cleanup in a way that doesn't require modifying the code at the point of
  freeing, and allows shared or garbage collected resources to be freed
  correctly."
  ([address size scope]
   (.asSegment ^MemoryAddress address size scope))
  ([address size scope cleanup]
   (.asSegment ^MemoryAddress address size cleanup scope)))

(defn add-close-action!
  "Adds a 0-arity function to be run when the `scope` closes."
  [scope action]
  (.addCloseAction ^ResourceScope scope action))

(defn clone-segment
  "Clones the content of `segment` into a new segment of the same size."
  ([segment] (clone-segment segment (connected-scope)))
  ([segment scope]
   (doto ^MemorySegment (alloc (.byteSize ^MemorySegment segment) scope)
     (.copyFrom ^MemorySegment segment))))

(defn slice-segments
  "Constructs a lazy seq of `size`-length memory segments, sliced from `segment`."
  [segment size]
  (let [num-segments (quot (.byteSize ^MemorySegment segment) size)]
    (map #(slice segment (* % size) size)
         (range num-segments))))

(def primitive-types
  "A set of keywords representing all the primitive types which may be passed to
  or returned from native functions."
  #{::byte ::short ::int ::long ::long-long
    ::char
    ::float ::double
    ::pointer ::void})

(defn- type-dispatch
  "Gets a type dispatch value from a (potentially composite) type."
  [type]
  (cond
    (qualified-keyword? type) type
    (sequential? type) (keyword (first type))))

(defmulti primitive-type
  "Gets the primitive type that is used to pass as an argument for the `type`.

  This is for objects which are passed to native functions as primitive types,
  but which need additional logic to be performed during serialization and
  deserialization.

  Returns nil for any type which does not have a primitive representation."
  type-dispatch)

(defmethod primitive-type :default
  [type]
  (primitive-types type))

(defmethod primitive-type ::pointer
  [_type]
  ::pointer)

(def c-prim-layout
  "Map of primitive type names to the [[CLinker]] types for a method handle."
  {::byte CLinker/C_CHAR
   ::short CLinker/C_SHORT
   ::int CLinker/C_INT
   ::long CLinker/C_LONG
   ::long-long CLinker/C_LONG_LONG
   ::char CLinker/C_CHAR
   ::float CLinker/C_FLOAT
   ::double CLinker/C_DOUBLE
   ::pointer CLinker/C_POINTER})

(defmulti c-layout
  "Gets the layout object for a given `type`.

  If a type is primitive it will return the appropriate primitive
  layout (see [[c-prim-layout]]).

  Otherwise, it should return a [[GroupLayout]] for the given type."
  type-dispatch)

(defmethod c-layout :default
  [type]
  (c-prim-layout (or (primitive-type type) type)))

(def java-prim-layout
  "Map of primitive type names to the Java types for a method handle."
  {::byte Byte/TYPE
   ::short Short/TYPE
   ::int Integer/TYPE
   ::long Long/TYPE
   ::long-long Long/TYPE
   ::char Byte/TYPE
   ::float Float/TYPE
   ::double Double/TYPE
   ::pointer MemoryAddress
   ::void Void/TYPE})

(defn java-layout
  "Gets the Java class to an argument of this type for a method handle.

  If a type serializes to a primitive it returns return a Java primitive type.
  Otherwise, it returns [[MemorySegment]]."
  [type]
  (java-prim-layout (or (primitive-type type) type) MemorySegment))

(defn size-of
  "The size in bytes of the given `type`."
  [type]
  (.byteSize ^MemoryLayout (c-layout type)))

(defn alloc-instance
  "Allocates a memory segment for the given `type`."
  ([type] (alloc-instance type (connected-scope)))
  ([type scope] (MemorySegment/allocateNative (long (size-of type)) ^ResourceScope scope)))

(declare serialize serialize-into)

(defmulti serialize*
  "Constructs a serialized version of the `obj` and returns it.

  Any new allocations made during the serialization should be tied to the given
  `scope`, except in extenuating circumstances.

  This method should only be implemented for types that serialize to primitives."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type scope]
    (type-dispatch type)))

(def ^:private primitive-cast
  "Map from primitive type names to the function to cast it to a primitive."
  {::byte byte
   ::short short
   ::int int
   ::long long
   ::long-long long
   ::char char
   ::float float
   ::double double})

(defmethod serialize* :default
  [obj type _scope]
  (if-let [prim (primitive-type type)]
    ((primitive-cast prim) obj)
    (throw (ex-info "Attempted to serialize a non-primitive type with primitive methods"
                    {:type type
                     :object obj}))))

(defmethod serialize* ::pointer
  [obj type scope]
  (when-not (null? obj)
    (if (sequential? type)
      (let [segment (alloc-instance (second type) scope)]
        (serialize-into obj (second type) segment scope)
        (address-of segment))
      obj)))

(defmulti serialize-into
  "Writes a serialized version of the `obj` to the given `segment`.

  Any new allocations made during the serialization should be tied to the given
  `scope`, except in extenuating circumstances.

  This method should be implemented for any type which does not
  override [[c-layout]].

  For any other type, this will serialize it as [[serialize*]] before writing
  the result value into the `segment`."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type segment scope]
    (type-dispatch type)))

(defmethod serialize-into :default
  [obj type segment scope]
  (if-some [prim-layout (primitive-type type)]
    (serialize-into (serialize* obj type scope) prim-layout segment scope)
    (throw (ex-info "Attempted to serialize an object to a type that has not been overriden"
                    {:type type
                     :object obj}))))

(defmethod serialize-into ::byte
  [obj _type segment _scope]
  (MemoryAccess/setByte segment (byte obj)))

(defmethod serialize-into ::short
  [obj _type segment _scope]
  (MemoryAccess/setShort segment (short obj)))

(defmethod serialize-into ::int
  [obj _type segment _scope]
  (MemoryAccess/setInt segment (int obj)))

(defmethod serialize-into ::long
  [obj _type segment _scope]
  (MemoryAccess/setLong segment (long obj)))

(defmethod serialize-into ::long-long
  [obj _type segment _scope]
  (MemoryAccess/setLong segment (long obj)))

(defmethod serialize-into ::char
  [obj _type segment _scope]
  (MemoryAccess/setChar segment (char obj)))

(defmethod serialize-into ::float
  [obj _type segment _scope]
  (MemoryAccess/setFloat segment (float obj)))

(defmethod serialize-into ::double
  [obj _type segment _scope]
  (MemoryAccess/setDouble segment (double obj)))

(defmethod serialize-into ::pointer
  [obj _type segment _scope]
  (MemoryAccess/setAddress segment obj))

(defn serialize
  "Serializes an arbitrary type.

  For types which have a primitive representation, this serializes into that
  representation. For types which do not, it allocates a new segment and
  serializes into that."
  ([obj type] (serialize obj type (connected-scope)))
  ([obj type scope]
   (if (primitive-type type)
     (serialize* obj type scope)
     (let [segment (alloc-instance type scope)]
       (serialize-into obj type segment scope)
       segment))))

(declare deserialize deserialize*)

(defmulti deserialize-from
  "Deserializes the given segment into a Clojure data structure.

  For types that serialize to primitives, a default implementation will
  deserialize the primitive before calling [[deserialize*]]."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [segment type]
    (type-dispatch type)))

(defmethod deserialize-from :default
  [segment type]
  (if-some [prim (primitive-type type)]
    (-> segment
        (deserialize-from prim)
        (deserialize* type))
    (throw (ex-info "Attempted to deserialize a non-primitive type that has not been overriden"
                    {:type type
                     :segment segment}))))

(defmethod deserialize-from ::byte
  [segment _type]
  (MemoryAccess/getByte segment))

(defmethod deserialize-from ::short
  [segment _type]
  (MemoryAccess/getShort segment))

(defmethod deserialize-from ::int
  [segment _type]
  (MemoryAccess/getInt segment))

(defmethod deserialize-from ::long
  [segment _type]
  (MemoryAccess/getLong segment))

(defmethod deserialize-from ::long-long
  [segment _type]
  (MemoryAccess/getLong segment))

(defmethod deserialize-from ::char
  [segment _type]
  (MemoryAccess/getChar segment))

(defmethod deserialize-from ::float
  [segment _type]
  (MemoryAccess/getFloat segment))

(defmethod deserialize-from ::double
  [segment _type]
  (MemoryAccess/getDouble segment))

(defmethod deserialize-from ::pointer
  [segment type]
  (cond-> (MemoryAccess/getAddress segment)
    (sequential? type) (deserialize* type)))

(defmulti deserialize*
  "Deserializes a primitive object into a Clojure data structure.

  This is intended for use with types that are returned as a primitive but which
  need additional processing before they can be returned."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type]
    (type-dispatch type)))

(defmethod deserialize* :default
  [obj type]
  (if (primitive-type type)
    obj
    (throw (ex-info "Attempted to deserialize a non-primitive type with primitive methods"
                    {:type type
                     :segment obj}))))

(defmethod deserialize* ::pointer
  [addr type]
  (when-not (null? addr)
    (if (sequential? type)
      (deserialize-from (slice-global addr (size-of (second type)))
                        (second type))
      addr)))

(defn deserialize
  "Deserializes an arbitrary type.

  For types which have a primitive representation, this deserializes the
  primitive representation. For types which do not, this deserializes out of
  a segment."
  [obj type]
  (when-not (identical? ::void type)
    ((if (primitive-type type)
       deserialize*
       deserialize-from)
     obj type)))

(defn seq-of
  "Constructs a lazy sequence of `type` elements deserialized from `segment`."
  [type segment]
  (map #(deserialize % type) (slice-segments segment (size-of type))))

;;; C String type

(defmethod primitive-type ::c-string
  [_type]
  ::pointer)

(defmethod serialize* ::c-string
  [obj _type scope]
  (if obj
    (address-of (CLinker/toCString ^String obj ^ResourceScope scope))
    (MemoryAddress/NULL)))

(defmethod deserialize* ::c-string
  [addr _type]
  (when-not (null? addr)
    (CLinker/toJavaString ^MemoryAddress addr)))

;;; Union types

(defmethod c-layout ::union
  [[_union _dispatch types & {:as _opts} :as _type]]
  (let [items (map c-layout types)]
    (MemoryLayout/unionLayout
     (into-array MemoryLayout items))))

(defmethod serialize-into ::union
  [obj [_union dispatch _types & {:keys [extract]}] segment scope]
  (let [type (dispatch obj)]
    (serialize-into
     (if extract
       (extract type obj)
       obj)
     type
     segment
     scope)))

(defmethod deserialize-from ::union
  [segment type]
  (clone-segment (slice segment 0 (size-of type))))

;;; Struct types

(defmethod c-layout ::struct
  [[_struct fields]]
  (let [fields (for [[field-name field] fields]
                 (.withName ^MemoryLayout (c-layout field)
                            (name field-name)))]
    (MemoryLayout/structLayout
     (into-array MemoryLayout fields))))

(defmethod serialize-into ::struct
  [obj [_struct fields] segment scope]
  (loop [offset 0
         fields fields]
    (when (seq fields)
      (let [[field type] (first fields)
            size (size-of type)]
        (serialize-into
         (get obj field) type
         (slice segment offset size) scope)
        (recur (long (+ offset size)) (rest fields))))))

(defmethod deserialize-from ::struct
  [segment [_struct fields]]
  (loop [offset 0
         fields fields
         obj {}]
    (if (seq fields)
      (let [[field type] (first fields)
            size (size-of type)]
        (recur
         (long (+ offset size))
         (rest fields)
         (assoc obj field (deserialize-from
                           (slice segment offset size)
                           type))))
      obj)))

;;; Padding type

(defmethod c-layout ::padding
  [[_padding size]]
  (MemoryLayout/paddingLayout (* 8 size)))

(defmethod serialize-into ::padding
  [_obj [_padding _size] _segment _scope]
  nil)

(defmethod deserialize-from ::padding
  [_segment [_padding _size]]
  nil)

;;; Array types

(defmethod c-layout ::array
  [[_array type count]]
  (MemoryLayout/sequenceLayout
   count
   (c-layout type)))

(defmethod serialize-into ::array
  [obj [_array type count] segment scope]
  (dorun
   (map #(serialize-into %1 type %2 scope)
        obj
        (slice-segments (slice segment 0 (* count (size-of type)))
                        (size-of type)))))

(defmethod deserialize-from ::array
  [segment [_array type count]]
  (map #(deserialize-from % type)
       (slice-segments (slice segment 0 (* count (size-of type)))
                       (size-of type))))

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
  ([args] (method-type args ::void))
  ([args ret]
   (MethodType/methodType
    ^Class (java-layout ret)
    ^"[Ljava.lang.Class;" (into-array Class (map java-layout args)))))

(defn- function-descriptor
  "Gets the [[FunctionDescriptor]] for a set of `args` and `ret` types."
  ([args] (function-descriptor args ::void))
  ([args ret]
   (let [args-arr (into-array MemoryLayout (map c-layout args))]
     (if-not (identical? ret ::void)
       (FunctionDescriptor/of
        (c-layout ret)
        args-arr)
       (FunctionDescriptor/ofVoid
        args-arr)))))

(defn- downcall-handle
  "Gets the [[MethodHandle]] for the function at the `address`."
  [address method-type function-descriptor]
  (.downcallHandle (CLinker/getInstance) address method-type function-descriptor))

(def ^:private load-instructions
  "Mapping from primitive types to the instruction used to load them onto the stack."
  {::byte :bload
   ::short :sload
   ::int :iload
   ::long :lload
   ::long-long :lload
   ::char :cload
   ::float :fload
   ::double :dload
   ::pointer :aload})

(def ^:private prim-classes
  "Mapping from primitive types to their box classes."
  {::byte Byte
   ::short Short
   ::int Integer
   ::long Long
   ::long-long Long
   ::char Character
   ::float Float
   ::double Double})

(defn- to-object-asm
  "Constructs a bytecode sequence to box a primitive on the top of the stack.

  If the `type` is not primitive, then no change will occur. If it is void, a
  null reference will be pushed to the stack."
  [type]
  (cond
    (identical? ::void type) [:ldc nil]
    (identical? ::pointer (primitive-type type)) []
    :else
    (let [prim-type (some-> type primitive-type)]
      (if-some [prim  (some-> prim-type name keyword)]
       ;; Box primitive
       [:invokestatic (prim-classes prim-type) "valueOf" [prim (prim-classes prim-type)]]
       ;; Return object without change
       []))))

(defn- insn-layout
  "Gets the type keyword or class for referring to the type in bytecode."
  [type]
  (if (some-> (primitive-type type) (not= ::pointer))
    (keyword (name type))
    (java-layout type)))

(def ^:private unbox-fn-for-type
  "Map from type name to the name of its unboxing function."
  {::byte "byteValue"
   ::short "shortValue"
   ::int "intValue"
   ::long "longValue"
   ::long-long "longValue"
   ::char "charValue"
   ::float "floatValue"
   ::double "doubleValue"})

(defn- to-prim-asm
  "Constructs a bytecode sequence to unbox a primitive type on top of the stack.

  If the `type` is not primitive, then no change will occur. If it is void, it
  will be popped."
  [type]
  (cond
    (identical? ::void type) [:pop]
    (identical? ::pointer (primitive-type type)) []
    :else
    (let [prim-type (some-> type primitive-type)]
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
                               (not (primitive-type ret)) inc)
                             Object)
               :emit [[:aload 0]
                      [:getfield :this "downcall_handle" MethodHandle]
                      (when-not (primitive-type ret)
                        [[:aload 1]
                         [:checkcast SegmentAllocator]])
                      (map-indexed
                       (fn [idx arg]
                         [[:aload (cond-> (inc idx)
                                    (not (primitive-type ret)) inc)]
                          (to-prim-asm arg)])
                       args)
                      [:invokevirtual MethodHandle "invokeExact"
                       (cond->>
                           (conj (mapv insn-layout args)
                                 (insn-layout ret))
                         (not (primitive-type ret)) (cons SegmentAllocator))]
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
    (address-of symbol-or-addr)
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

(defn make-serde-wrapper
  "Constructs a wrapper function for the `downcall` which serializes the arguments
  and deserializes the return value."
  [downcall arg-types ret-type]
  (if (primitive-type ret-type)
    (fn native-fn [& args]
      (with-open [scope (stack-scope)]
        (deserialize
         (apply downcall (map #(serialize %1 %2 scope) args arg-types))
         ret-type)))
    (fn native-fn [& args]
      (with-open [scope (stack-scope)]
        (deserialize
         (apply downcall (scope-allocator scope)
                (map #(serialize %1 %2 scope) args arg-types))
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
      (make-serde-wrapper args ret)))

;;; Function types

(def ^:private return-for-type
  "Map from type name to the return instruction for that type."
  {::byte :breturn
   ::short :sreturn
   ::int :ireturn
   ::long :lreturn
   ::long-long :lreturn
   ::char :creturn
   ::float :freturn
   ::double :dreturn
   ::void :return})

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
              :desc (conj (mapv java-layout arg-types)
                          (java-layout ret-type))
              :emit [[:aload 0]
                     [:getfield :this "upcall_ifn" IFn]
                     (map-indexed
                      (fn [idx arg]
                        [[(load-instructions arg) (inc idx)]
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

(defmethod primitive-type ::fn
  [_type]
  ::pointer)

(defn- upcall-serde-wrapper
  "Creates a function that wraps `f` which deserializes the arguments and
  serializes the return type in the [[global-scope]]."
  [f arg-types ret-type]
  (fn [& args]
    (serialize
     (apply f (map deserialize args arg-types))
     ret-type
     (global-scope))))

(defmethod serialize* ::fn
  [f [_fn arg-types ret-type & {:keys [raw-fn?]}] scope]
  (.upcallStub
   (CLinker/getInstance)
   (cond-> f
     (not raw-fn?) (upcall-serde-wrapper arg-types ret-type)
     :always (upcall-handle arg-types ret-type))
   (function-descriptor arg-types ret-type)
   scope))

(defmethod deserialize* ::fn
  [addr [_fn arg-types ret-type & {:keys [raw-fn?]}]]
  (-> addr
      (downcall-handle
       (method-type arg-types ret-type)
       (function-descriptor arg-types ret-type))
      (downcall-fn arg-types ret-type)
      (cond->
        (not raw-fn?) (make-serde-wrapper arg-types ret-type))))

;;; Static memory access

(defn const
  "Gets the value of a constant stored in `symbol-or-addr`."
  [symbol-or-addr type]
  (deserialize (ensure-address symbol-or-addr) [::pointer type]))

(deftype StaticVariable [addr type meta]
  Addressable
  (address [_]
    addr)
  IDeref
  (deref [_]
    (deserialize addr [::pointer type]))

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
  (serialize-into
   newval (.-type static-var)
   (slice-global (.-addr static-var) (size-of (.-type static-var)))
   (global-scope))
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

(defmethod reify-symbolspec :downcall
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

(s/def ::type
  (s/spec
   (s/nonconforming
    (s/or :simple-type qualified-keyword?
          :complex-type (s/cat :base-type qualified-keyword?
                               :type-args (s/* any?))))))

(s/def ::defcfn-args
  (s/and
   (s/cat :name simple-symbol?
          :doc (s/? string?)
          :attr-map (s/? map?)
          :symbol (s/nonconforming
                   (s/or :string string?
                         :symbol simple-symbol?))
          :native-arglist (s/coll-of ::type :kind vector?)
          :return-type qualified-keyword?
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
        args-types (gensym "args-types")
        ret-type (gensym "ret-type")
        invoke (gensym "invoke")
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
    `(let [~args-types ~(:native-arglist args)
           ~ret-type ~(:return-type args)
           ~invoke (make-downcall ~(name (:symbol args)) ~args-types ~ret-type)
           ~(or (-> args :wrapper :native-fn) native-sym)
           ~(if (and (every? #(= % (primitive-type %))
                             (:native-arglist args))
                     (= (:return-type args)
                        (primitive-type (:return-type args))))
              invoke
              `(make-serde-wrapper ~invoke ~args-types ~ret-type))
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
                                   (mapv (comp symbol name)
                                         (:native-arglist args)))))))
                   (:attr-map args)))
         ~@(list (:doc args))
         fun#))))
(s/fdef defcfn
  :args ::defcfn-args)

(defmacro defalias
  "Defines a type alias from `new-type` to `aliased-type`.

  This creates needed serialization and deserialization implementations for the
  aliased type."
  {:style/indent [:defn]}
  [new-type aliased-type]
  (if (primitive-type aliased-type)
    `(let [aliased# ~aliased-type]
       (defmethod primitive-type ~new-type
         [_type#]
         (primitive-type aliased#))
       (defmethod serialize* ~new-type
         [obj# _type# scope#]
         (serialize* obj# aliased# scope#))
       (defmethod deserialize* ~new-type
         [obj# _type#]
         (deserialize* obj# aliased#)))
    `(let [aliased# ~aliased-type]
       (defmethod c-layout ~new-type
         [_type#]
         (c-layout aliased#))
       (defmethod serialize-into ~new-type
         [obj# _type# segment# scope#]
         (serialize-into obj# aliased# segment# scope#))
       (defmethod deserialize-from ~new-type
         [segment# _type#]
         (deserialize-from segment# aliased#)))))
(s/fdef defalias
  :args (s/cat :new-type qualified-keyword?
               :aliased-type ::type))
