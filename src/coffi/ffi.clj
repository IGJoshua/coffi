(ns coffi.ffi
  (:refer-clojure :exclude [defstruct])
  (:import
   (java.lang.invoke VarHandle)
   (jdk.incubator.foreign
    CLinker
    FunctionDescriptor
    MemoryAccess
    MemoryAddress
    MemoryHandles
    MemoryLayout
    MemoryLayout$PathElement
    MemoryLayouts
    MemorySegment
    ResourceScope)))

(defmulti serialize*
  "Writes a serialized version of the `obj` to the given `segment`.

  Any new allocations made during the serialization should be tied to the given
  `scope`, except in extenuating circumstances."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type segment scope]
    type))

(defmethod serialize* ::byte
  [obj _type segment _scope]
  (MemoryAccess/setByte segment (byte obj)))

(defmethod serialize* ::short
  [obj _type segment _scope]
  (MemoryAccess/setShort segment (short obj)))

(defmethod serialize* ::int
  [obj _type segment _scope]
  (MemoryAccess/setInt segment (int obj)))

(defmethod serialize* ::long
  [obj _type segment _scope]
  (MemoryAccess/setLong segment (long obj)))

(defmethod serialize* ::long-long
  [obj _type segment _scope]
  (MemoryAccess/setLong segment (long obj)))

(defmethod serialize* ::char
  [obj _type segment _scope]
  (MemoryAccess/setChar segment (char obj)))

(defmethod serialize* ::float
  [obj _type segment _scope]
  (MemoryAccess/setFloat segment (float obj)))

(defmethod serialize* ::double
  [obj _type segment _scope]
  (MemoryAccess/setDouble segment (double obj)))

(defmethod serialize* ::pointer
  [obj _type segment _scope]
  (MemoryAccess/setAddress segment obj))

(defmulti deserialize
  "Deserializes the given segment into a Clojure data structure."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [segment type]
    type))

(defmethod deserialize ::byte
  [segment _type]
  (MemoryAccess/getByte segment))

(defmethod deserialize ::short
  [segment _type]
  (MemoryAccess/getShort segment))

(defmethod deserialize ::int
  [segment _type]
  (MemoryAccess/getInt segment))

(defmethod deserialize ::long
  [segment _type]
  (MemoryAccess/getLong segment))

(defmethod deserialize ::long-long
  [segment _type]
  (MemoryAccess/getLong segment))

(defmethod deserialize ::char
  [segment _type]
  (MemoryAccess/getChar segment))

(defmethod deserialize ::float
  [segment _type]
  (MemoryAccess/getFloat segment))

(defmethod deserialize ::double
  [segment _type]
  (MemoryAccess/getDouble segment))

(defmethod deserialize ::pointer
  [segment _type]
  (MemoryAccess/getAddress segment))

(defmulti size-of
  "The size in bytes of the given `type`."
  (fn [type] type))

(defmethod size-of ::byte
  [_]
  Byte/SIZE)

(defmethod size-of ::short
  [_]
  Short/SIZE)

(defmethod size-of ::int
  [_]
  Integer/SIZE)

(defmethod size-of ::long
  [_]
  Long/SIZE)

(defmethod size-of ::long-long
  [_]
  Long/SIZE)

(defmethod size-of ::char
  [_]
  Byte/SIZE)

(defmethod size-of ::float
  [_]
  Float/SIZE)

(defmethod size-of ::double
  [_]
  Double/SIZE)

(defmethod size-of ::pointer
  [_]
  (.byteSize MemoryLayouts/ADDRESS))

(def c-layout
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

(def java-layout
  "Map of primitive type names to the Java types for a method handle."
  {::byte Byte/TYPE
   ::short Short/TYPE
   ::int Integer/TYPE
   ::long Long/TYPE
   ::long-long Long/TYPE
   ::char Byte/TYPE
   ::float Float/TYPE
   ::double Double/TYPE
   ::pointer MemoryAddress})

(defn alloc
  "Allocates `size` bytes."
  ([size] (alloc size (ResourceScope/newImplicitScope)))
  ([size scope] (MemorySegment/allocateNative size scope)))

(defn alloc-instance
  "Allocates a memory segment for the given `type`."
  ([type] (alloc-instance type (ResourceScope/newImplicitScope)))
  ([type scope] (MemorySegment/allocateNative (size-of type) scope)))

(defn serialize
  "Serializes the `obj` into a newly-allocated [[MemorySegment]]."
  ([obj type] (serialize obj type (ResourceScope/newImplicitScope)))
  ([obj type scope] (serialize* obj type (alloc-instance type scope) scope)))

(defn stack-scope
  "Constructs a new scope for use only in this thread.

  The memory allocated within this scope is cheap to allocate, like a native
  stack."
  []
  (ResourceScope/newConfinedScope))

(defn shared-scope
  "Constructs a new shared scope.

  This scope can be shared across threads and memory allocated in it will only
  be cleaned up once every thread accessing the scope closes it."
  []
  (ResourceScope/newSharedScope))

(comment
  ;;; Prospective syntax for ffi

  ;; This function has no out params, and no extra marshalling work, so it has no
  ;; body
  (defcfun strlen
    "Counts the number of bytes in a C String."
    "strlen" [::c-string] ::int)

  ;; This function has an output parameter and requires some clojure code to
  ;; translate the values from the c fn to something sensible in clojure.
  (defcfun some-func
    "Gets some output value"
    "someFunc" [::pointer] ::int
    []
    (let [out-int (alloc-instance ::int)
          success? (zero? (some-func (.address out-int)))]
      (if success?
        (deserialize ::int out-int)
        (throw (ex-info (getErrorString) {})))))

  ;; This function probably wouldn't actually get wrapped, since the cost of
  ;; marshalling is greater than the speed boost of using an in-place sort. That
  ;; said, this is a nice sample of what more complex marshalling looks like.
  (defcfun qsort
    "Quicksort implementation"
    "qsort"
    [::pointer ::long ::long (fn [::pointer ::pointer] ::int)]
    ::void
    [type comparator list]
    (let [copied-list (alloc (* (count list) (size-of type)))
          _ (for [segment (seq-of type copied-list)]
              (serialize type segment))
          comp-fn (fn [addr1 addr2]
                    (let [obj1 (deserialize type (slice-global addr1 (size-of type)))
                          obj2 (deserialize type (slice-global addr2 (size-of type)))]
                      (comparator obj1 obj2)))]
      (qsort copied-list (count list) (size-of type) comp-fn)
      (for [segment (seq-of type copied-list)]
        (deserialize type segment))))
  )
