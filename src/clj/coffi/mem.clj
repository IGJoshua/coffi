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
   (java.nio ByteOrder)
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
  (^MemorySegment [size] (alloc size (connected-scope)))
  (^MemorySegment [size scope] (MemorySegment/allocateNative (long size) ^ResourceScope scope)))

(defn alloc-with
  "Allocates `size` bytes using the `allocator`."
  (^MemorySegment [allocator size]
   (.allocate ^SegmentAllocator allocator (long size)))
  (^MemorySegment [allocator size alignment]
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
  ^MemoryAddress [addressable]
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
  ^MemorySegment [address size]
  (.asSlice (MemorySegment/globalNativeSegment)
            ^MemoryAddress address (long size)))

(defn slice
  "Get a slice over the `segment` with the given `offset`."
  (^MemorySegment [segment offset]
   (.asSlice ^MemorySegment segment (long offset)))
  (^MemorySegment [segment offset size]
   (.asSlice ^MemorySegment segment (long offset) (long size))))

(defn slice-into
  "Get a slice into the `segment` starting at the `address`."
  (^MemorySegment [address segment]
   (.asSlice ^MemorySegment segment ^MemoryAddress address))
  (^MemorySegment [address segment size]
   (.asSlice ^MemorySegment segment ^MemoryAddress address (long size))))

(defn with-offset
  "Get a new address `offset` from the old `address`."
  ^MemoryAddress [address offset]
  (.addOffset ^MemoryAddress address (long offset)))

(defn as-segment
  "Dereferences an `address` into a memory segment associated with the `scope`.

  If `cleanup` is provided, it is a 0-arity function run when the scope is
  closed. This can be used to register a free method for the memory, or do other
  cleanup in a way that doesn't require modifying the code at the point of
  freeing, and allows shared or garbage collected resources to be freed
  correctly."
  (^MemorySegment [^MemoryAddress address size scope]
   (.asSegment address (long size) scope))
  (^MemorySegment [^MemoryAddress address size ^ResourceScope scope cleanup]
   (.asSegment address (long size) cleanup scope)))

(defn add-close-action!
  "Adds a 0-arity function to be run when the `scope` closes."
  [^ResourceScope scope ^Runnable action]
  (.addCloseAction scope action)
  nil)

(defn copy-segment
  "Copies the content to `dest` from `src`.

  Returns `dest`."
  ^MemorySegment [^MemorySegment dest ^MemorySegment src]
  (with-acquired (map segment-scope [src dest])
    (.copyFrom dest src)
    dest))

(defn clone-segment
  "Clones the content of `segment` into a new segment of the same size."
  (^MemorySegment [segment] (clone-segment segment (connected-scope)))
  (^MemorySegment [^MemorySegment segment scope]
   (with-acquired [(segment-scope segment) scope]
     (copy-segment ^MemorySegment (alloc (.byteSize segment) scope) segment))))

(defn slice-segments
  "Constructs a lazy seq of `size`-length memory segments, sliced from `segment`."
  [^MemorySegment segment size]
  (let [num-segments (quot (.byteSize segment) size)]
    (map #(slice segment (* % size) size)
         (range num-segments))))

(def big-endian
  "The big-endian [[ByteOrder]].

  See [[little-endian]], [[native-endian]]."
  ByteOrder/BIG_ENDIAN)

(def little-endian
  "The little-endian [[ByteOrder]].

  See [[big-endian]], [[native-endian]]"
  ByteOrder/LITTLE_ENDIAN)

(def native-endian
  "The [[ByteOrder]] for the native endianness of the current hardware.

  See [[big-endian]], [[little-endian]]."
  (ByteOrder/nativeOrder))

(def byte-layout
  "The [[MemoryLayout]] for a byte in [[native-endian]] [[ByteOrder]]."
  CLinker/C_CHAR)

(def short-layout
  "The [[MemoryLayout]] for a c-sized short in [[native-endian]] [[ByteOrder]]."
  CLinker/C_SHORT)

(def int-layout
  "The [[MemoryLayout]] for a c-sized int in [[native-endian]] [[ByteOrder]]."
  CLinker/C_INT)

(def long-layout
  "The [[MemoryLayout]] for a c-sized long in [[native-endian]] [[ByteOrder]]."
  CLinker/C_LONG)

(def long-long-layout
  "The [[MemoryLayout]] for a c-sized long-long in [[native-endian]] [[ByteOrder]]."
  CLinker/C_LONG_LONG)

(def char-layout
  "The [[MemoryLayout]] for a c-sized char in [[native-endian]] [[ByteOrder]]."
  CLinker/C_CHAR)

(def float-layout
  "The [[MemoryLayout]] for a c-sized float in [[native-endian]] [[ByteOrder]]."
  CLinker/C_FLOAT)

(def double-layout
  "The [[MemoryLayout]] for a c-sized double in [[native-endian]] [[ByteOrder]]."
  CLinker/C_DOUBLE)

(def pointer-layout
  "The [[MemoryLayout]] for a native pointer in [[native-endian]] [[ByteOrder]]."
  CLinker/C_POINTER)

(defn read-byte
  "Reads a [[byte]] from the `segment`, at an optional `offset`."
  {:inline
   (fn read-byte-inline
     ([segment]
      `(MemoryAccess/getByte ~segment))
     ([segment offset]
      `(MemoryAccess/getByteAtOffset ~segment ~offset)))}
  ([^MemorySegment segment]
   (MemoryAccess/getByte segment))
  ([^MemorySegment segment ^long offset]
   (MemoryAccess/getByteAtOffset segment offset)))

(defn read-short
  "Reads a [[short]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-short-inline
     ([segment]
      `(MemoryAccess/getShort ~segment))
     ([segment offset]
      `(MemoryAccess/getShortAtOffset ~segment ~offset))
     ([segment offset byte-order]
      `(MemoryAccess/getShortAtOffset ~segment ~offset ~byte-order)))}
  ([^MemorySegment segment]
   (MemoryAccess/getShort segment))
  ([^MemorySegment segment ^long offset]
   (MemoryAccess/getShortAtOffset segment offset))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (MemoryAccess/getShortAtOffset segment offset byte-order)))

(defn read-int
  "Reads a [[int]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-int-inline
     ([segment]
      `(MemoryAccess/getInt ~segment))
     ([segment offset]
      `(MemoryAccess/getIntAtOffset ~segment ~offset))
     ([segment offset byte-order]
      `(MemoryAccess/getIntAtOffset ~segment ~offset ~byte-order)))}
  ([^MemorySegment segment]
   (MemoryAccess/getInt segment))
  ([^MemorySegment segment ^long offset]
   (MemoryAccess/getIntAtOffset segment offset))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (MemoryAccess/getIntAtOffset segment offset byte-order)))

(defn read-long
  "Reads a [[long]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-long-inline
     ([segment]
      `(MemoryAccess/getLong ~segment))
     ([segment offset]
      `(MemoryAccess/getLongAtOffset ~segment ~offset))
     ([segment offset byte-order]
      `(MemoryAccess/getLongAtOffset ~segment ~offset ~byte-order)))}
  (^long [^MemorySegment segment]
   (MemoryAccess/getLong segment))
  (^long [^MemorySegment segment ^long offset]
   (MemoryAccess/getLongAtOffset segment offset))
  (^long [^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (MemoryAccess/getLongAtOffset segment offset byte-order)))

(defn read-char
  "Reads a [[char]] from the `segment`, at an optional `offset`."
  {:inline
   (fn read-char-inline
     ([segment]
      `(char (Byte/toUnsignedInt (MemoryAccess/getByte ~segment))))
     ([segment offset]
      `(char (Byte/toUnsignedInt (MemoryAccess/getByteAtOffset ~segment ~offset)))))}
  ([^MemorySegment segment]
   (char (Byte/toUnsignedInt (MemoryAccess/getByte segment))))
  ([^MemorySegment segment ^long offset]
   (char (Byte/toUnsignedInt (MemoryAccess/getByteAtOffset segment offset)))))

(defn read-float
  "Reads a [[float]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-float-inline
     ([segment]
      `(MemoryAccess/getFloat ~segment))
     ([segment offset]
      `(MemoryAccess/getFloatAtOffset ~segment ~offset))
     ([segment offset byte-order]
      `(MemoryAccess/getFloatAtOffset ~segment ~offset ~byte-order)))}
  ([^MemorySegment segment]
   (MemoryAccess/getFloat segment))
  ([^MemorySegment segment ^long offset]
   (MemoryAccess/getFloatAtOffset segment offset))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (MemoryAccess/getFloatAtOffset segment offset byte-order)))

(defn read-double
  "Reads a [[double]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-double-inline
     ([segment]
      `(MemoryAccess/getDouble ~segment))
     ([segment offset]
      `(MemoryAccess/getDoubleAtOffset ~segment ~offset))
     ([segment offset byte-order]
      `(MemoryAccess/getDoubleAtOffset ~segment ~offset ~byte-order)))}
  (^double [^MemorySegment segment]
   (MemoryAccess/getDouble segment))
  (^double [^MemorySegment segment ^long offset]
   (MemoryAccess/getDoubleAtOffset segment offset))
  (^double [^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (MemoryAccess/getDoubleAtOffset segment offset byte-order)))

(defn write-byte
  "Writes a [[byte]] to the `segment`, at an optional `offset`."
  {:inline
   (fn write-byte-inline
     ([segment value]
      `(MemoryAccess/setByte ~segment ~value))
     ([segment offset value]
      `(MemoryAccess/setByteAtOffset ~segment ~offset ~value)))}
  ([^MemorySegment segment value]
   (MemoryAccess/setByte segment ^byte value))
  ([^MemorySegment segment ^long offset value]
   (MemoryAccess/setByteAtOffset segment offset ^byte value)))

(defn write-short
  "Writes a [[short]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-short-inline
     ([segment value]
      `(MemoryAccess/setShort ~segment ~value))
     ([segment offset value]
      `(MemoryAccess/setShortAtOffset ~segment ~offset ~value))
     ([segment offset byte-order value]
      `(MemoryAccess/setShortAtOffset ~segment ~offset ~byte-order ~value)))}
  ([^MemorySegment segment value]
   (MemoryAccess/setShort segment ^short value))
  ([^MemorySegment segment ^long offset value]
   (MemoryAccess/setShortAtOffset segment offset ^short value))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order value]
   (MemoryAccess/setShortAtOffset segment offset byte-order ^short value)))

(defn write-int
  "Writes a [[int]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-int-inline
     ([segment value]
      `(MemoryAccess/setInt ~segment ~value))
     ([segment offset value]
      `(MemoryAccess/setIntAtOffset ~segment ~offset ~value))
     ([segment offset byte-order value]
      `(MemoryAccess/setIntAtOffset ~segment ~offset ~byte-order ~value)))}
  ([^MemorySegment segment value]
   (MemoryAccess/setInt segment ^int value))
  ([^MemorySegment segment ^long offset value]
   (MemoryAccess/setIntAtOffset segment offset ^int value))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order value]
   (MemoryAccess/setIntAtOffset segment offset byte-order ^int value)))

(defn write-long
  "Writes a [[long]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-long-inline
     ([segment value]
      `(MemoryAccess/setLong ~segment ~value))
     ([segment offset value]
      `(MemoryAccess/setLongAtOffset ~segment ~offset ~value))
     ([segment offset byte-order value]
      `(MemoryAccess/setLongAtOffset ~segment ~offset ~byte-order ~value)))}
  (^long [^MemorySegment segment ^long value]
   (MemoryAccess/setLong segment value))
  (^long [^MemorySegment segment ^long offset ^long value]
   (MemoryAccess/setLongAtOffset segment offset value))
  (^long [^MemorySegment segment ^long offset ^ByteOrder byte-order ^long value]
   (MemoryAccess/setLongAtOffset segment offset byte-order value)))

(defn write-char
  "Writes a [[char]] to the `segment`, at an optional `offset`."
  {:inline
   (fn write-char-inline
     ([segment value]
      `(MemoryAccess/setByte ~segment (unchecked-byte (unchecked-int ~value))))
     ([segment offset value]
      `(MemoryAccess/setByteAtOffset ~segment ~offset (unchecked-byte (unchecked-int ~value)))))}
  ([^MemorySegment segment value]
   (MemoryAccess/setByte
    segment
    ;; HACK(Joshua): The Clojure runtime doesn't have an unchecked-byte cast for
    ;;               characters, so this double cast is necessary unless I emit
    ;;               my own bytecode with insn.
    (unchecked-byte (unchecked-int ^char value))))
  ([^MemorySegment segment ^long offset value]
   (MemoryAccess/setByteAtOffset segment offset (unchecked-byte (unchecked-int ^char value)))))

(defn write-float
  "Writes a [[float]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-float-inline
     ([segment value]
      `(MemoryAccess/setFloat ~segment ~value))
     ([segment offset value]
      `(MemoryAccess/setFloatAtOffset ~segment ~offset ~value))
     ([segment offset byte-order value]
      `(MemoryAccess/setFloatAtOffset ~segment ~offset ~byte-order ~value)))}
  ([^MemorySegment segment value]
   (MemoryAccess/setFloat segment ^float value))
  ([^MemorySegment segment ^long offset value]
   (MemoryAccess/setFloatAtOffset segment offset ^float value))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order value]
   (MemoryAccess/setFloatAtOffset segment offset byte-order ^float value)))

(defn write-double
  "Writes a [[double]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-double-inline
     ([segment value]
      `(MemoryAccess/setDouble ~segment ~value))
     ([segment offset value]
      `(MemoryAccess/setDoubleAtOffset ~segment ~offset ~value))
     ([segment offset byte-order value]
      `(MemoryAccess/setDoubleAtOffset ~segment ~offset ~byte-order ~value)))}
  (^double [^MemorySegment segment ^double value]
   (MemoryAccess/setDouble segment value))
  (^double [^MemorySegment segment ^long offset ^double value]
   (MemoryAccess/setDoubleAtOffset segment offset value))
  (^double [^MemorySegment segment ^long offset ^ByteOrder byte-order ^double value]
   (MemoryAccess/setDoubleAtOffset segment offset byte-order value)))

(defn- type-dispatch
  "Gets a type dispatch value from a (potentially composite) type."
  [type]
  (cond
    (qualified-keyword? type) type
    (sequential? type) (keyword (first type))))

(def primitive?
  "A set of all primitive types."
  #{::byte ::short ::int ::long ::long-long
    ::char ::float ::double ::pointer})

(defmulti primitive-type
  "Gets the primitive type that is used to pass as an argument for the `type`.

  This is for objects which are passed to native functions as primitive types,
  but which need additional logic to be performed during serialization and
  deserialization.

  Implementations of this method should take into account that type arguments
  may not always be evaluated before passing to this function.

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

(defmulti c-layout
  "Gets the layout object for a given `type`.

  If a type is primitive it will return the appropriate primitive
  layout (see [[c-prim-layout]]).

  Otherwise, it should return a [[GroupLayout]] for the given type."
  type-dispatch)

(defmethod c-layout :default
  [type]
  (c-layout (primitive-type type)))

(defmethod c-layout ::byte
  [_type]
  byte-layout)

(defmethod c-layout ::short
  [_type]
  short-layout)

(defmethod c-layout ::int
  [_type]
  int-layout)

(defmethod c-layout ::long
  [_type]
  long-layout)

(defmethod c-layout ::long-long
  [_type]
  long-long-layout)

(defmethod c-layout ::char
  [_type]
  char-layout)

(defmethod c-layout ::float
  [_type]
  float-layout)

(defmethod c-layout ::double
  [_type]
  double-layout)

(defmethod c-layout ::pointer
  [_type]
  pointer-layout)

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
  ^Class [type]
  (java-prim-layout (or (primitive-type type) type) MemorySegment))

(defn size-of
  "The size in bytes of the given `type`."
  ^long [type]
  (let [t (cond-> type
            (not (instance? MemoryLayout type)) c-layout)]
    (.byteSize ^MemoryLayout t)))

(defn align-of
  "The alignment in bytes of the given `type`."
  ^long [type]
  (let [t (cond-> type
            (not (instance? MemoryLayout type)) c-layout)]
    (.byteAlignment ^MemoryLayout t)))

(defn alloc-instance
  "Allocates a memory segment for the given `type`."
  (^MemorySegment [type] (alloc-instance type (connected-scope)))
  (^MemorySegment [type scope] (MemorySegment/allocateNative ^long (size-of type) ^ResourceScope scope)))

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

(defmethod serialize* :default
  [obj type _scope]
  (throw (ex-info "Attempted to serialize a non-primitive type with primitive methods"
                  {:type type
                   :object obj})))

(defmethod serialize* ::byte
  [obj _type _scope]
  (byte obj))

(defmethod serialize* ::short
  [obj _type _scope]
  (short obj))

(defmethod serialize* ::int
  [obj _type _scope]
  (int obj))

(defmethod serialize* ::long
  [obj _type _scope]
  (long obj))

(defmethod serialize* ::long-long
  [obj _type _scope]
  (long obj))

(defmethod serialize* ::char
  [obj _type _scope]
  (char obj))

(defmethod serialize* ::float
  [obj _type _scope]
  (float obj))

(defmethod serialize* ::double
  [obj _type _scope]
  (double obj))

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

(defmethod serialize* ::void
  [_obj _type _scope]
  nil)

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
  (write-byte segment (byte obj)))

(defmethod serialize-into ::short
  [obj type segment _scope]
  (if (sequential? type)
    (write-short segment 0 (second type) (short obj))
    (write-short segment (short obj))))

(defmethod serialize-into ::int
  [obj type segment _scope]
  (if (sequential? type)
    (write-int segment 0 (second type) (int obj))
    (write-int segment (int obj))))

(defmethod serialize-into ::long
  [obj type segment _scope]
  (if (sequential? type)
    (write-long segment 0 (second type) (long obj))
    (write-long segment (long obj))))

(defmethod serialize-into ::long-long
  [obj type segment _scope]
  (if (sequential? type)
    (write-long segment 0 (second type) (long obj))
    (write-long segment (long obj))))

(defmethod serialize-into ::char
  [obj _type segment _scope]
  (write-char segment (char obj)))

(defmethod serialize-into ::float
  [obj type segment _scope]
  (if (sequential? type)
    (write-float segment 0 (second type) (float obj))
    (write-float segment (float obj))))

(defmethod serialize-into ::double
  [obj type segment _scope]
  (if (sequential? type)
    (write-double segment 0 (second type) (double obj))
    (write-double segment (double obj))))

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
  (read-byte segment))

(defmethod deserialize-from ::short
  [segment type]
  (if (sequential? type)
    (read-short segment 0 (second type))
    (read-short segment)))

(defmethod deserialize-from ::int
  [segment type]
  (if (sequential? type)
    (read-int segment 0 (second type))
    (read-int segment)))

(defmethod deserialize-from ::long
  [segment type]
  (if (sequential? type)
    (read-long segment 0 (second type))
    (read-long segment)))

(defmethod deserialize-from ::long-long
  [segment type]
  (if (sequential? type)
    (read-long segment 0 (second type))
    (read-long segment)))

(defmethod deserialize-from ::char
  [segment _type]
  (read-char segment))

(defmethod deserialize-from ::float
  [segment type]
  (if (sequential? type)
    (read-float segment 0 (second type))
    (read-float segment)))

(defmethod deserialize-from ::double
  [segment type]
  (if (sequential? type)
    (read-double segment 0 (second type))
    (read-double segment)))

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

(defmethod deserialize* :default
  [obj type]
  (throw (ex-info "Attempted to deserialize a non-primitive type with primitive methods"
                  {:type type
                   :segment obj})))

(defmethod deserialize* ::byte
  [obj _type]
  obj)

(defmethod deserialize* ::short
  [obj _type]
  obj)

(defmethod deserialize* ::int
  [obj _type]
  obj)

(defmethod deserialize* ::long
  [obj _type]
  obj)

(defmethod deserialize* ::long-long
  [obj _type]
  obj)

(defmethod deserialize* ::char
  [obj _type]
  obj)

(defmethod deserialize* ::float
  [obj _type]
  obj)

(defmethod deserialize* ::double
  [obj _type]
  obj)

(defmethod deserialize* ::pointer
  [addr type]
  (when-not (null? addr)
    (if (sequential? type)
      (deserialize-from (slice-global addr (size-of (second type)))
                        (second type))
      addr)))

(defmethod deserialize* ::void
  [_obj _type]
  nil)

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
  (if (and (s/valid? ::type aliased-type)
           (primitive-type aliased-type))
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
               :aliased-type any?))
