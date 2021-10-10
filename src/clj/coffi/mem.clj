(ns coffi.mem
  "Functions for managing native allocations, resource scopes, and (de)serialization.

  For any new type to be implemented, three multimethods must be overriden, but
  which three depends on the native representation of the type.

  If the native representation of the type is a primitive (whether or not other
  data beyond the primitive is associated with it, as e.g. a pointer),
  then [[primitive-type]] must be overriden to return which primitive type it is
  serialized as, then [[serialize*]] and [[deserialize*]] should be overriden.

  If the native representation of the type is a composite type, like a union,
  struct, or array, then [[c-layout]] must be overriden to return the native
  layout of the type, and [[serialize-into]] and [[deserialize-from]] should be
  overriden to allow marshaling values of the type into and out of memory
  segments.

  When writing code that manipulates a segment, it's best practice to
  use [[with-acquired]] on the [[segment-scope]] in order to ensure it won't be
  released during its manipulation."
  (:require
   [clojure.spec.alpha :as s])
  (:import
   (jdk.incubator.foreign
    Addressable
    CLinker
    MemoryAccess
    MemoryAddress
    MemoryLayout
    MemorySegment
    ResourceScope
    ResourceScope$Handle
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

(defn segment-scope
  "Gets the scope used to construct the `segment`."
  ^ResourceScope [segment]
  (.scope ^MemorySegment segment))

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
  "Acquires one or more `scopes` until the `body` completes.

  This is only necessary to do on shared scopes, however if you are operating on
  an arbitrary passed scope, it is best practice to wrap code that interacts
  with it wrapped in this."
  {:style/indent 1}
  [scopes & body]
  `(let [scopes# (vec ~scopes)
         handles# (mapv #(.acquire ^ResourceScope %) scopes#)]
     (try ~@body
          (finally
            (doseq [idx# (range (count scopes#))
                    :let [scope# (nth scopes# idx#)
                          handle# (nth handles# idx#)]]
              (.release ^ResourceScope scope#
                        ^ResourceScope$Handle handle#))))))
(s/fdef with-acquired
  :args (s/cat :scopes any?
               :body (s/* any?)))

(defn address-of
  "Gets the address of a given segment.

  This value can be used as an argument to functions which take a pointer."
  [addressable]
  (.address ^Addressable addressable))

(defn null?
  "Checks if a memory address is null."
  [addr]
  (or (.equals (MemoryAddress/NULL) addr) (not addr)))

(defn address?
  "Checks if an object is a memory address.

  `nil` is considered an address."
  [addr]
  (or (nil? addr) (instance? MemoryAddress addr)))

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

(defn copy-segment
  "Copies the content to `dest` from `src`"
  [dest src]
  (with-acquired (map segment-scope [src dest])
    (.copyFrom ^MemorySegment dest ^MemorySegment src)))

(defn clone-segment
  "Clones the content of `segment` into a new segment of the same size."
  ([segment] (clone-segment segment (connected-scope)))
  ([segment scope]
   (with-acquired [(segment-scope segment) scope]
     (doto ^MemorySegment (alloc (.byteSize ^MemorySegment segment) scope)
       (copy-segment segment)))))

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
  [_type]
  nil)

(defmethod primitive-type ::byte
  [_type]
  ::byte)

(defmethod primitive-type ::short
  [_type]
  ::short)

(defmethod primitive-type ::int
  [_type]
  ::int)

(defmethod primitive-type ::long
  [_type]
  ::long)

(defmethod primitive-type ::long-long
  [_type]
  ::long-long)

(defmethod primitive-type ::char
  [_type]
  ::char)

(defmethod primitive-type ::float
  [_type]
  ::float)

(defmethod primitive-type ::double
  [_type]
  ::double)

(defmethod primitive-type ::pointer
  [_type]
  ::pointer)

(defmethod primitive-type ::void
  [_type]
  ::void)

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

;; TODO(Joshua): For performance, turn this into a bunch of specific defmethods
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

;; TODO(Joshua): For performance, turn this into a bunch of specific defmethods
(defmethod serialize* :default
  [obj type _scope]
  (if-let [prim (primitive-type type)]
    (when-not (= ::void prim)
      ((primitive-cast prim) obj))
    (throw (ex-info "Attempted to serialize a non-primitive type with primitive methods"
                    {:type type
                     :object obj}))))

(defmethod serialize* ::pointer
  [obj type scope]
  (if-not (null? obj)
    (if (sequential? type)
      (with-acquired [scope]
        (let [segment (alloc-instance (second type) scope)]
          (serialize-into obj (second type) segment scope)
          (address-of segment)))
      obj)
    (MemoryAddress/NULL)))

(defmulti serialize-into
  "Writes a serialized version of the `obj` to the given `segment`.

  Any new allocations made during the serialization should be tied to the given
  `scope`, except in extenuating circumstances.

  This method should be implemented for any type which does not
  override [[c-layout]].

  For any other type, this will serialize it as [[serialize*]] before writing
  the result value into the `segment`.

  Implementations of this should be inside a [[with-acquired]] block for the
  `scope` if they perform multiple memory operations."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type segment scope]
    (type-dispatch type)))

;; TODO(Joshua): For performance, turn this into a bunch of specific defmethods
(defmethod serialize-into :default
  [obj type segment scope]
  (if-some [prim-layout (primitive-type type)]
    (with-acquired [(segment-scope segment) scope]
      (serialize-into (serialize* obj type scope) prim-layout segment scope))
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
  [obj type segment scope]
  (with-acquired [(segment-scope segment) scope]
    (MemoryAccess/setAddress
     segment
     (cond-> obj
       (sequential? type) (serialize* type scope)))))

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
  deserialize the primitive before calling [[deserialize*]].

  Implementations of this should be inside a [[with-acquired]] block for the the
  `segment`'s scope if they perform multiple memory operations."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [segment type]
    (type-dispatch type)))

;; TODO(Joshua): For performance, turn this into a bunch of specific defmethods
(defmethod deserialize-from :default
  [segment type]
  (if-some [prim (primitive-type type)]
    (with-acquired [(segment-scope segment)]
      (-> segment
          (deserialize-from prim)
          (deserialize* type)))
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
  (with-acquired [(segment-scope segment)]
    (cond-> (MemoryAccess/getAddress segment)
      (sequential? type) (deserialize* type))))

(defmulti deserialize*
  "Deserializes a primitive object into a Clojure data structure.

  This is intended for use with types that are returned as a primitive but which
  need additional processing before they can be returned."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type]
    (type-dispatch type)))

;; TODO(Joshua): For performance, turn this into a bunch of specific defmethods
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
    (if (primitive-type type)
      (deserialize* obj type)
      (deserialize-from obj type))))

(defn seq-of
  "Constructs a lazy sequence of `type` elements deserialized from `segment`."
  [type segment]
  (with-acquired [(segment-scope segment)]
    (map #(deserialize % type) (slice-segments segment (size-of type)))))

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
  [[_union types & {:as _opts} :as _type]]
  (let [items (map c-layout types)]
    (MemoryLayout/unionLayout
     (into-array MemoryLayout items))))

(defmethod serialize-into ::union
  [obj [_union _types & {:keys [dispatch extract]} :as type] segment scope]
  (when-not dispatch
    (throw (ex-info "Attempted to serialize a union with no dispatch function"
                    {:type type
                     :value obj})))
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
  (mapv #(deserialize-from % type)
        (slice-segments (slice segment 0 (* count (size-of type)))
                        (size-of type))))

(s/def ::type
  (s/spec
   (s/nonconforming
    (s/or :simple-type qualified-keyword?
          :complex-type (s/cat :base-type qualified-keyword?
                               :type-args (s/* any?))))))

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
