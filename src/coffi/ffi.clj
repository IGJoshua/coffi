(ns coffi.ffi
  (:refer-clojure :exclude [defstruct])
  (:import
   (java.lang.invoke VarHandle)
   (jdk.incubator.foreign
    MemoryAccess
    MemoryAddress
    MemoryHandles
    MemoryLayout
    MemoryLayout$PathElement
    MemoryLayouts
    MemorySegment
    FunctionDescriptor)))

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
