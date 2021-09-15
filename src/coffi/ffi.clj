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
  (fn [type obj segment scope]
    type))

(defmulti deserialize
  "Deserializes the given object into a Clojure data structure."
  (fn [type obj]
    type))

(defmulti size-of
  "The size in bytes of the given `type`."
  (fn [type] type))

(defmethod size-of ::byte
  [type]
  Byte/SIZE)

(defmethod size-of ::short
  [type]
  Short/SIZE)

(defmethod size-of ::integer
  [type]
  Integer/SIZE)

(defmethod size-of ::long
  [type]
  Long/SIZE)

(defmethod size-of ::long-long
  [type]
  Long/SIZE)

(defmethod size-of ::char
  [type]
  Byte/SIZE)

(defmethod size-of ::float
  [type]
  Float/SIZE)

(defmethod size-of ::double
  [type]
  Double/SIZE)

(defmethod size-of ::pointer
  [type]
  (.byteSize MemoryLayouts/ADDRESS))

(def c-layout
  "Map of primitive type names to the [[CLinker]] types for a method handle."
  {::byte CLinker/C_CHAR
   ::short CLinker/C_SHORT
   ::integer CLinker/C_INT
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
   ::integer Integer/TYPE
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
  ([type obj] (serialize type obj (ResourceScope/newImplicitScope)))
  ([type obj scope] (serialize* type obj (alloc-instance type scope) scope)))

(comment
  ;;; Prospective syntax for ffi

  ;; This function has no out params, and no extra marshalling work, so it has no
  ;; body
  (defcfun strlen
    "Counts the number of bytes in a C String."
    "strlen" [::c-string] ::integer)
  ​
  ;; This function has an output parameter and requires some clojure code to
  ;; translate the values from the c fn to something sensible in clojure.
  (defcfun some-func
    "Gets some output value"
    "someFunc" [::pointer] ::integer
    []
    (let [out-int (alloc-instance ::integer)
          success? (zero? (some-func out-int))]
      (if success?
        (deserialize ::integer out-int)
        (throw (ex-info (getErrorString) {})))))
  ​
  ;; This function probably wouldn't actually get wrapped, since the cost of
  ;; marshalling is greater than the speed boost of using an in-place sort. That
  ;; said, this is a nice sample of what more complex marshalling looks like.
  (defcfun qsort
    "Quicksort implementation"
    "qsort"
    [::pointer ::long ::long (fn [::pointer ::pointer] ::integer)]
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
