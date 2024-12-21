(ns coffi.mem
  "Functions for managing native allocations, memory arenas, and (de)serialization.

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
  segments."
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s])
  (:import
   (java.lang.foreign
    AddressLayout
    Arena
    MemoryLayout
    MemorySegment
    MemorySegment$Scope
    SegmentAllocator
    ValueLayout
    ValueLayout$OfByte
    ValueLayout$OfShort
    ValueLayout$OfInt
    ValueLayout$OfLong
    ValueLayout$OfChar
    ValueLayout$OfFloat
    ValueLayout$OfDouble)
   (java.lang.ref Cleaner)
   (java.util.function Consumer)
   (java.nio ByteOrder)))

(set! *warn-on-reflection* true)

(defn confined-arena
  "Constructs a new arena for use only in this thread.

  The memory allocated within this arena is cheap to allocate, like a native
  stack.

  The memory allocated within this arena will be cleared once it is closed, so
  it is usually a good idea to create it in a [[with-open]] clause."
  (^Arena []
   (Arena/ofConfined)))

(defn shared-arena
  "Constructs a new shared memory arena.

  This arena can be shared across threads and memory allocated in it will only
  be cleaned up once any thread accessing the arena closes it."
  (^Arena []
   (Arena/ofShared)))

(defn auto-arena
  "Constructs a new memory arena that is managed by the garbage collector.

  The arena may be shared across threads, and all resources created with it will
  be cleaned up at the same time, when all references have been collected.

  This type of arena cannot be closed, and therefore should not be created in
  a [[with-open]] clause."
  ^Arena []
  (Arena/ofAuto))

(defn global-arena
  "Constructs the global arena, which will never reclaim its resources.

  This arena may be shared across threads, but is intended mainly in cases where
  memory is allocated with [[alloc]] but is either never freed or whose
  management is relinquished to a native library, such as when returned from a
  callback."
  ^Arena []
  (Arena/global))

(defn arena-allocator
  "Constructs a [[SegmentAllocator]] from the given [[Arena]].

  This is primarily used when working with unwrapped downcall functions. When a
  downcall function returns a non-primitive type, it must be provided with an
  allocator."
  ^SegmentAllocator [^Arena arena]
  (reify SegmentAllocator
    (^MemorySegment allocate [_this ^long byte-size ^long byte-alignment]
      (.allocate arena ^long byte-size ^long byte-alignment))))

(defn alloc
  "Allocates `size` bytes.

  If an `arena` is provided, the allocation will be reclaimed when it is closed."
  (^MemorySegment [size] (alloc size (auto-arena)))
  (^MemorySegment [size arena] (.allocate ^Arena arena (long size)))
  (^MemorySegment [size alignment arena] (.allocate ^Arena arena (long size) (long alignment))))

(defn alloc-with
  "Allocates `size` bytes using the `allocator`."
  (^MemorySegment [allocator size]
   (.allocate ^SegmentAllocator allocator (long size)))
  (^MemorySegment [allocator size alignment]
   (.allocate ^SegmentAllocator allocator (long size) (long alignment))))

(defn address-of
  "Gets the address of a given segment as a number."
  ^long [addressable]
  (.address ^MemorySegment addressable))

(def ^MemorySegment null
  "The NULL pointer object.

  While this object is safe to pass to functions which serialize to a pointer,
  it's generally encouraged to simply pass `nil`. This value primarily exists to
  make it easier to write custom types with a primitive pointer representation."
  MemorySegment/NULL)

(defn null?
  "Checks if a memory address is null."
  [addr]
  (or (.equals null addr) (not addr)))

(defn address?
  "Checks if an object is a memory address.

  `nil` is considered an address."
  [addr]
  (or (nil? addr) (instance? MemorySegment addr)))

(defn slice
  "Get a slice over the `segment` with the given `offset`."
  (^MemorySegment [segment offset]
   (.asSlice ^MemorySegment segment (long offset)))
  (^MemorySegment [segment offset size]
   (.asSlice ^MemorySegment segment (long offset) (long size))))

(defn reinterpret
  "Reinterprets the `segment` as having the passed `size`.

  If `arena` is passed, the scope of the `segment` is associated with the arena,
  as well as its access constraints. If `cleanup` is passed, it will be a
  1-argument function of a fresh memory segment backed by the same memory as the
  returned segment which should perform any required cleanup operations. It will
  be called when the `arena` is closed."
  (^MemorySegment [^MemorySegment segment size]
   (.reinterpret segment (long size) (auto-arena) nil))
  (^MemorySegment [^MemorySegment segment size ^Arena arena]
   (.reinterpret segment (long size) arena nil))
  (^MemorySegment [^MemorySegment segment size ^Arena arena cleanup]
   (.reinterpret segment (long size) arena
                 (reify Consumer
                   (accept [_this segment]
                     (cleanup segment))))))

(defn as-segment
  "Dereferences an `address` into a memory segment associated with the `arena` (default global)."
  (^MemorySegment [^long address]
   (MemorySegment/ofAddress address))
  (^MemorySegment [^long address size]
   (reinterpret (MemorySegment/ofAddress address) size))
  (^MemorySegment [^long address size ^Arena arena]
   (reinterpret (MemorySegment/ofAddress address) (long size) arena nil))
  (^MemorySegment [^long address size ^Arena arena cleanup]
   (reinterpret (MemorySegment/ofAddress address) (long size) arena cleanup)))

(defn copy-segment
  "Copies the content to `dest` from `src`.

  Returns `dest`."
  ^MemorySegment [^MemorySegment dest ^MemorySegment src]
  (.copyFrom dest src))

(defn clone-segment
  "Clones the content of `segment` into a new segment of the same size."
  (^MemorySegment [segment] (clone-segment segment (auto-arena)))
  (^MemorySegment [^MemorySegment segment ^Arena arena]
   (copy-segment ^MemorySegment (alloc (.byteSize segment) arena) segment)))

(defn slice-segments
  "Constructs a lazy seq of `size`-length memory segments, sliced from `segment`."
  [^MemorySegment segment size]
  (let [num-segments (quot (.byteSize segment) size)]
    (map #(slice segment (* % size) size)
         (range num-segments))))

(def ^ByteOrder big-endian
  "The big-endian [[ByteOrder]].

  See [[little-endian]], [[native-endian]]."
  ByteOrder/BIG_ENDIAN)

(def ^ByteOrder little-endian
  "The little-endian [[ByteOrder]].

  See [[big-endian]], [[native-endian]]"
  ByteOrder/LITTLE_ENDIAN)

(def ^ByteOrder native-endian
  "The [[ByteOrder]] for the native endianness of the current hardware.

  See [[big-endian]], [[little-endian]]."
  (ByteOrder/nativeOrder))

(def ^ValueLayout$OfByte byte-layout
  "The [[MemoryLayout]] for a byte in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_BYTE)

(def ^ValueLayout$OfShort short-layout
  "The [[MemoryLayout]] for a c-sized short in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_SHORT)

(def ^ValueLayout$OfInt int-layout
  "The [[MemoryLayout]] for a c-sized int in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_INT)

(def ^ValueLayout$OfLong long-layout
  "The [[MemoryLayout]] for a c-sized long in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_LONG)

(def ^ValueLayout$OfByte char-layout
  "The [[MemoryLayout]] for a c-sized char in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_BYTE)

(def ^ValueLayout$OfFloat float-layout
  "The [[MemoryLayout]] for a c-sized float in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_FLOAT)

(def ^ValueLayout$OfDouble double-layout
  "The [[MemoryLayout]] for a c-sized double in [[native-endian]] [[ByteOrder]]."
  ValueLayout/JAVA_DOUBLE)

(def ^AddressLayout pointer-layout
  "The [[MemoryLayout]] for a native pointer in [[native-endian]] [[ByteOrder]]."
  ValueLayout/ADDRESS)

(def ^long short-size
  "The size in bytes of a c-sized short."
  (.byteSize short-layout))

(def ^long int-size
  "The size in bytes of a c-sized int."
  (.byteSize int-layout))

(def ^long long-size
  "The size in bytes of a c-sized long."
  (.byteSize long-layout))

(def ^long float-size
  "The size in bytes of a c-sized float."
  (.byteSize float-layout))

(def ^long double-size
  "The size in bytes of a c-sized double."
  (.byteSize double-layout))

(def ^long pointer-size
  "The size in bytes of a c-sized pointer."
  (.byteSize pointer-layout))

(def ^long short-alignment
  "The alignment in bytes of a c-sized short."
  (.byteAlignment short-layout))

(def ^long int-alignment
  "The alignment in bytes of a c-sized int."
  (.byteAlignment int-layout))

(def ^long long-alignment
  "The alignment in bytes of a c-sized long."
  (.byteAlignment long-layout))

(def ^long float-alignment
  "The alignment in bytes of a c-sized float."
  (.byteAlignment float-layout))

(def ^long double-alignment
  "The alignment in bytes of a c-sized double."
  (.byteAlignment double-layout))

(def ^long pointer-alignment
  "The alignment in bytes of a c-sized pointer."
  (.byteAlignment pointer-layout))

(defn read-byte
  "Reads a [[byte]] from the `segment`, at an optional `offset`."
  {:inline
   (fn read-byte-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^ValueLayout$OfByte byte-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^ValueLayout$OfByte byte-layout offset#))))}
  ([^MemorySegment segment]
   (.get segment ^ValueLayout$OfByte byte-layout 0))
  ([^MemorySegment segment ^long offset]
   (.get segment ^ValueLayout$OfByte byte-layout offset)))

(defn read-short
  "Reads a [[short]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-short-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^ValueLayout$OfShort short-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^ValueLayout$OfShort short-layout offset#)))
     ([segment offset byte-order]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order]
         (.get ^MemorySegment segment# (.withOrder ^ValueLayout$OfShort short-layout ^ByteOrder byte-order#) offset#))))}
  ([^MemorySegment segment]
   (.get segment ^ValueLayout$OfShort short-layout 0))
  ([^MemorySegment segment ^long offset]
   (.get segment ^ValueLayout$OfShort short-layout offset))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (.get segment (.withOrder ^ValueLayout$OfShort short-layout byte-order) offset)))

(defn read-int
  "Reads a [[int]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-int-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^ValueLayout$OfInt int-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^ValueLayout$OfInt int-layout offset#)))
     ([segment offset byte-order]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order]
         (.get ^MemorySegment segment# (.withOrder ^ValueLayout$OfInt int-layout ^ByteOrder byte-order#) offset#))))}
  ([^MemorySegment segment]
   (.get segment ^ValueLayout$OfInt int-layout 0))
  ([^MemorySegment segment ^long offset]
   (.get segment ^ValueLayout$OfInt int-layout offset))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (.get segment (.withOrder ^ValueLayout$OfInt int-layout byte-order) offset)))

(defn read-long
  "Reads a [[long]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-long-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^ValueLayout$OfLong long-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^ValueLayout$OfLong long-layout offset#)))
     ([segment offset byte-order]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order]
         (.get ^MemorySegment segment# (.withOrder ^ValueLayout$OfLong long-layout ^ByteOrder byte-order#) offset#))))}
  (^long [^MemorySegment segment]
   (.get segment ^ValueLayout$OfLong long-layout 0))
  (^long [^MemorySegment segment ^long offset]
   (.get segment ^ValueLayout$OfLong long-layout offset))
  (^long [^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (.get segment (.withOrder ^ValueLayout$OfLong long-layout byte-order) offset)))

(defn read-char
  "Reads a [[char]] from the `segment`, at an optional `offset`."
  {:inline
   (fn read-char-inline
     ([segment]
      `(let [segment# ~segment]
         (char (Byte/toUnsignedInt (.get ^MemorySegment segment# ^ValueLayout$OfByte byte-layout 0)))))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (char (Byte/toUnsignedInt (.get ^MemorySegment segment# ^ValueLayout$OfByte byte-layout offset#))))))}
  ([^MemorySegment segment]
   (char (Byte/toUnsignedInt (.get segment ^ValueLayout$OfChar byte-layout 0))))
  ([^MemorySegment segment ^long offset]
   (char (Byte/toUnsignedInt (.get segment ^ValueLayout$OfChar byte-layout offset)))))

(defn read-float
  "Reads a [[float]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-float-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^ValueLayout$OfFloat float-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^ValueLayout$OfFloat float-layout offset#)))
     ([segment offset byte-order]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order]
         (.get ^MemorySegment segment# (.withOrder ^ValueLayout$OfFloat float-layout ^ByteOrder byte-order#) offset#))))}
  ([^MemorySegment segment]
   (.get segment ^ValueLayout$OfFloat float-layout 0))
  ([^MemorySegment segment ^long offset]
   (.get segment ^ValueLayout$OfFloat float-layout offset))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (.get segment (.withOrder ^ValueLayout$OfFloat float-layout byte-order) offset)))

(defn read-double
  "Reads a [[double]] from the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn read-double-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^ValueLayout$OfDouble double-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^ValueLayout$OfDouble double-layout offset#)))
     ([segment offset byte-order]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order]
         (.get ^MemorySegment segment# (.withOrder ^ValueLayout$OfDouble double-layout ^ByteOrder byte-order#) offset#))))}
  (^double [^MemorySegment segment]
   (.get segment ^ValueLayout$OfDouble double-layout 0))
  (^double [^MemorySegment segment ^long offset]
   (.get segment ^ValueLayout$OfDouble double-layout offset))
  (^double [^MemorySegment segment ^long offset ^ByteOrder byte-order]
   (.get segment (.withOrder ^ValueLayout$OfDouble double-layout byte-order) offset)))

(defn read-address
  "Reads an address from the `segment`, at an optional `offset`, wrapped in a [[MemorySegment]]."
  {:inline
   (fn read-address-inline
     ([segment]
      `(let [segment# ~segment]
         (.get ^MemorySegment segment# ^AddressLayout pointer-layout 0)))
     ([segment offset]
      `(let [segment# ~segment
             offset# ~offset]
         (.get ^MemorySegment segment# ^AddressLayout pointer-layout offset#))))}
  (^MemorySegment [^MemorySegment segment]
   (.get segment ^AddressLayout pointer-layout 0))
  (^MemorySegment [^MemorySegment segment ^long offset]
   (.get segment ^AddressLayout pointer-layout offset)))

(defn write-byte
  "Writes a [[byte]] to the `segment`, at an optional `offset`."
  {:inline
   (fn write-byte-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfByte byte-layout 0 value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfByte byte-layout offset# value#))))}
  ([^MemorySegment segment value]
   (.set segment ^ValueLayout$OfByte byte-layout 0 ^byte value))
  ([^MemorySegment segment ^long offset value]
   (.set segment ^ValueLayout$OfByte byte-layout offset ^byte value)))

(defn write-short
  "Writes a [[short]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-short-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfShort short-layout 0 value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfShort short-layout offset# value#)))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (.set ^MemorySegment segment# (.withOrder ^ValueLayout$OfShort short-layout ^ByteOrder byte-order#) offset# value#))))}
  ([^MemorySegment segment value]
   (.set segment ^ValueLayout$OfShort short-layout 0 ^short value))
  ([^MemorySegment segment ^long offset value]
   (.set segment ^ValueLayout$OfShort short-layout offset ^short value))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order value]
   (.set segment (.withOrder ^ValueLayout$OfShort short-layout byte-order) offset ^short value)))

(defn write-int
  "Writes a [[int]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-int-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfInt int-layout 0 value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfInt int-layout offset# value#)))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (.set ^MemorySegment segment# (.withOrder ^ValueLayout$OfInt int-layout ^ByteOrder byte-order#) offset# value#))))}
  ([^MemorySegment segment value]
   (.set segment ^ValueLayout$OfInt int-layout 0 ^int value))
  ([^MemorySegment segment ^long offset value]
   (.set segment ^ValueLayout$OfInt int-layout offset ^int value))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order value]
   (.set segment (.withOrder ^ValueLayout$OfInt int-layout byte-order) offset ^int value)))

(defn write-long
  "Writes a [[long]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-long-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfLong long-layout 0 value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfLong long-layout offset# value#)))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (.set ^MemorySegment segment# (.withOrder ^ValueLayout$OfLong long-layout ^ByteOrder byte-order#) offset# value#))))}
  (^long [^MemorySegment segment ^long value]
   (.set segment ^ValueLayout$OfLong long-layout 0 value))
  (^long [^MemorySegment segment ^long offset ^long value]
   (.set segment ^ValueLayout$OfLong long-layout offset value))
  (^long [^MemorySegment segment ^long offset ^ByteOrder byte-order ^long value]
   (.set segment (.withOrder ^ValueLayout$OfLong long-layout byte-order) offset value)))

(defn write-char
  "Writes a [[char]] to the `segment`, at an optional `offset`."
  {:inline
   (fn write-char-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfByte byte-layout 0 (unchecked-byte (unchecked-int value#)))))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfByte byte-layout offset# (unchecked-byte (unchecked-int value#))))))}
  ([^MemorySegment segment value]
   (.set
    segment
    ;; HACK(Joshua): The Clojure runtime doesn't have an unchecked-byte cast for
    ;;               characters, so this double cast is necessary unless I emit
    ;;               my own bytecode with insn.
    ^ValueLayout$OfByte byte-layout 0
    (unchecked-byte (unchecked-int ^char value))))
  ([^MemorySegment segment ^long offset value]
   (.set segment ^ValueLayout$OfByte byte-layout offset (unchecked-byte (unchecked-int ^char value)))))

(defn write-float
  "Writes a [[float]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-float-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfFloat float-layout 0 value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfFloat float-layout offset# value#)))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (.set ^MemorySegment segment# (.withOrder ^ValueLayout$OfFloat float-layout ^ByteOrder byte-order#) offset# value#))))}
  ([^MemorySegment segment value]
   (.set segment ^ValueLayout$OfFloat float-layout 0 ^float value))
  ([^MemorySegment segment ^long offset value]
   (.set segment ^ValueLayout$OfFloat float-layout offset ^float value))
  ([^MemorySegment segment ^long offset ^ByteOrder byte-order value]
   (.set segment (.withOrder ^ValueLayout$OfFloat float-layout byte-order) offset ^float value)))

(defn write-double
  "Writes a [[double]] to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-double-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfDouble double-layout 0 value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^ValueLayout$OfDouble double-layout offset# value#)))
     ([segment offset byte-order value]
      `(let [segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             value# ~value]
         (.set ^MemorySegment segment# (.withOrder ^ValueLayout$OfDouble double-layout ^ByteOrder byte-order#) offset# value#))))}
  (^double [^MemorySegment segment ^double value]
   (.set segment ^ValueLayout$OfDouble double-layout 0 value))
  (^double [^MemorySegment segment ^long offset ^double value]
   (.set segment ^ValueLayout$OfDouble double-layout offset value))
  (^double [^MemorySegment segment ^long offset ^ByteOrder byte-order ^double value]
   (.set segment (.withOrder ^ValueLayout$OfDouble double-layout byte-order) offset value)))

(defn write-address
  "Writes the address of the [[MemorySegment]] `value` to the `segment`, at an optional `offset`."
  {:inline
   (fn write-address-inline
     ([segment value]
      `(let [segment# ~segment
             value# ~value]
         (.set ^MemorySegment segment# ^AddressLayout pointer-layout 0 ^MemorySegment value#)))
     ([segment offset value]
      `(let [segment# ~segment
             offset# ~offset
             value# ~value]
         (.set ^MemorySegment segment# ^AddressLayout pointer-layout offset# ^MemorySegment value#))))}
  ([^MemorySegment segment ^MemorySegment value]
   (.set segment ^AddressLayout pointer-layout 0 value))
  ([^MemorySegment segment ^long offset ^MemorySegment value]
   (.set segment ^AddressLayout pointer-layout offset value)))

(defn write-bytes
  "Writes n elements from a [[byte]] array to the `segment`, at an optional `offset`."
  {:inline
   (fn write-byte-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'bytes})]
         (MemorySegment/copy value# 0  ^MemorySegment segment# ^ValueLayout$OfByte byte-layout 0 n#)))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'bytes})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfByte byte-layout offset# n#))))}
  ([^MemorySegment segment n ^bytes value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfByte byte-layout 0 ^int n))
  ([^MemorySegment segment n offset ^bytes value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfByte byte-layout ^long offset ^int n)))

(defn write-shorts
  "Writes n elements from a [[short]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-shorts-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'shorts})]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfShort short-layout 0 n#)))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'shorts})]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfShort short-layout ^long offset# n#))))}
  ([^MemorySegment segment n ^shorts value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfShort short-layout 0 ^int n))
  ([^MemorySegment segment n ^long offset ^shorts value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfShort short-layout ^long offset ^int n)))

(defn write-ints
  "Writes n elements from an [[int]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-ints-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'shorts})]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfInt int-layout 0 n#)))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'shorts})]
         (MemorySegment/copy value# 0 segment# ^ValueLayout$OfInt int-layout ^long offset# n#)))
     )}
  ([^MemorySegment segment n ^ints value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfInt int-layout 0 ^int n))
  ([^MemorySegment segment n ^long offset ^ints value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfInt int-layout ^long offset ^int n))
  )

(defn write-longs
  "Writes n elements from a [[long]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-longs-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'longs})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfLong long-layout 0 n#)
         ))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'longs})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfLong long-layout ^long offset# n#)
         ))
     )}
  ([^MemorySegment segment n ^longs value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfLong long-layout 0 ^int n))
  ([^MemorySegment segment n ^long offset ^longs value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfLong long-layout ^long offset ^int n))
  )


(defn write-chars
  "Writes n elements from a [[char]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-chars-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'chars})]
         (MemorySegment/copy (bytes (byte-array (map unchecked-int value#))) 0 segment# ^ValueLayout$OfChar char-layout 0 n#)))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'chars})]
         (MemorySegment/copy (bytes (byte-array (map unchecked-int value#))) 0 segment# ^ValueLayout$OfChar char-layout ^long offset# n#)))
     )}
  ([^MemorySegment segment n ^chars value]
   (MemorySegment/copy (bytes (byte-array (map unchecked-int value))) 0 segment ^ValueLayout$OfChar char-layout 0 ^int n))
  ([^MemorySegment segment n ^long offset ^chars value]
   (MemorySegment/copy (bytes (byte-array (map unchecked-int value))) 0 segment ^ValueLayout$OfChar char-layout ^long offset ^int n )))

(defn write-floats
  "Writes n elements from a [[float]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-floats-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'floats})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfFloat float-layout 0 n#)))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'floats})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfFloat float-layout ^long offset# n#))))}
  ([^MemorySegment segment n ^floats value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfFloat float-layout 0 ^int n))
  ([^MemorySegment segment n ^long offset ^floats value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfFloat float-layout ^long offset ^int n)))

(defn write-doubles
  "Writes n elements from a [[double]] array to the `segment`, at an optional `offset`.

  If `byte-order` is not provided, it defaults to [[native-endian]]."
  {:inline
   (fn write-doubles-inline
     ([segment n value]
      `(let [n# ~n
             segment# ~segment
             value# ~(with-meta value {:tag 'doubles})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfDouble double-layout 0 n#)))
     ([segment n offset value]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             value# ~(with-meta value {:tag 'doubles})]
         (MemorySegment/copy value# 0 ^MemorySegment segment# ^ValueLayout$OfDouble double-layout ^long offset# n#))))}
  ([^MemorySegment segment n ^doubles value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfDouble double-layout 0 ^int n))
  ([^MemorySegment segment n ^long offset ^doubles value]
   (MemorySegment/copy value 0 segment ^ValueLayout$OfDouble double-layout ^long offset ^int n)))







(defn read-bytes
  "reads `n` elements from a `segment` to a [[byte]] array, at an optional `offset`."
  {:inline
   (fn read-bytes-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (byte-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfByte byte-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (byte-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfByte byte-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (byte-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfByte byte-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (byte-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfByte byte-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (byte-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfByte byte-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (byte-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfByte byte-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn read-shorts
  "reads `n` elements from a `segment` to a [[short]] array, at an optional `offset`."
  {:inline
   (fn read-shorts-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (short-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfShort short-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (short-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfShort short-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (short-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfShort short-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (short-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfShort short-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (short-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfShort short-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (short-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfShort short-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn read-longs
  "reads `n` elements from a `segment` to a [[long]] array, at an optional `offset`."
  {:inline
   (fn read-longs-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (long-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfLong long-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (long-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfLong long-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (long-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfLong long-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (long-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfLong long-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (long-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfLong long-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (long-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfLong long-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn read-ints
  "reads `n` elements from a `segment` to a [[int]] array, at an optional `offset`."
  {:inline
   (fn read-ints-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (int-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfInt int-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (int-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfInt int-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (int-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfInt int-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (int-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfInt int-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (int-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfInt int-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (int-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfInt int-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn read-chars
  "reads `n` elements from a `segment` to a [[char]] array, at an optional `offset`."
  {:inline
   (fn read-chars-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (char-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfChar char-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (char-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfChar char-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (char-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfChar char-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (char-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfChar char-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (char-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfChar char-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (char-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfChar char-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn read-floats
  "reads `n` elements from a `segment` to a [[float]] array, at an optional `offset`."
  {:inline
   (fn read-floats-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (float-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfFloat float-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (float-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfFloat float-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (float-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfFloat float-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (float-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfFloat float-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (float-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfFloat float-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (float-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfFloat float-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn read-doubles
  "reads `n` elements from a `segment` to a [[double]] array, at an optional `offset`."
  {:inline
   (fn read-doubles-inline
     ([segment n]
      `(let [n# ~n
             segment# ~segment
             arr# (double-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfDouble double-layout 0 arr# 0 n#)
         arr#))
     ([segment n offset]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             arr# (double-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# ^ValueLayout$OfDouble double-layout offset# arr# 0 n#)
         arr#))
     ([segment n offset byte-order]
      `(let [n# ~n
             segment# ~segment
             offset# ~offset
             byte-order# ~byte-order
             arr# (double-array ~n)]
         (MemorySegment/copy ^MemorySegment segment# (.withOrder ^ValueLayout$OfDouble double-layout ^ByteOrder byte-order#) offset# arr# 0 n#)
         arr#)))}
  ([^MemorySegment segment n]
   (let [arr (double-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfDouble double-layout 0 arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset]
   (let [arr (double-array n)]
     (MemorySegment/copy segment ^ValueLayout$OfDouble double-layout offset arr 0 ^int n)
     arr))
  ([^MemorySegment segment n ^long offset ^ByteOrder byte-order]
   (let [arr (double-array n)]
     (MemorySegment/copy segment (.withOrder ^ValueLayout$OfDouble double-layout byte-order) offset arr 0 ^int n)
     arr)))

(defn- type-dispatch
  "Gets a type dispatch value from a (potentially composite) type."
  [type]
  (cond
    (qualified-keyword? type) type
    (sequential? type) (keyword (first type))
    :else (throw (ex-info "Invalid type object" {:type type}))))

(def primitive-types
  "A set of all primitive types."
  #{::byte ::short ::int ::long
    ::char ::float ::double ::pointer})

(defn primitive?
  "A predicate to determine if a given type is primitive."
  [type]
  (contains? primitive-types (type-dispatch type)))

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
  [type]
  (if (sequential? type)
    (.withOrder short-layout ^ByteOrder (second type))
    short-layout))

(defmethod c-layout ::int
  [type]
  (if (sequential? type)
    (.withOrder int-layout ^ByteOrder (second type))
    int-layout))

(defmethod c-layout ::long
  [type]
  (if (sequential? type)
    (.withOrder long-layout ^ByteOrder (second type))
    long-layout))

(defmethod c-layout ::char
  [_type]
  char-layout)

(defmethod c-layout ::float
  [type]
  (if (sequential? type)
    (.withOrder float-layout ^ByteOrder (second type))
    float-layout))

(defmethod c-layout ::double
  [type]
  (if (sequential? type)
    (.withOrder double-layout ^ByteOrder (second type))
    double-layout))

(defmethod c-layout ::pointer
  [_type]
  pointer-layout)

(def java-prim-layout
  "Map of primitive type names to the Java types for a method handle."
  {::byte Byte/TYPE
   ::short Short/TYPE
   ::int Integer/TYPE
   ::long Long/TYPE
   ::char Byte/TYPE
   ::float Float/TYPE
   ::double Double/TYPE
   ::pointer MemorySegment
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
  (^MemorySegment [type] (alloc-instance type (auto-arena)))
  (^MemorySegment [type arena] (.allocate ^Arena arena ^long (size-of type) ^long (align-of type))))

(declare serialize serialize-into)

(defmulti serialize*
  "Constructs a serialized version of the `obj` and returns it.

  Any new allocations made during the serialization should be tied to the given
  `arena`, except in extenuating circumstances.

  This method should only be implemented for types that serialize to primitives."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type arena]
    (type-dispatch type)))

(defmethod serialize* :default
  [obj type _arena]
  (throw (ex-info "Attempted to serialize a non-primitive type with primitive methods"
                  {:type type
                   :object obj})))

(defmethod serialize* ::byte
  [obj _type _arena]
  (byte obj))

(defmethod serialize* ::short
  [obj _type _arena]
  (short obj))

(defmethod serialize* ::int
  [obj _type _arena]
  (int obj))

(defmethod serialize* ::long
  [obj _type _arena]
  (long obj))

(defmethod serialize* ::char
  [obj _type _arena]
  (char obj))

(defmethod serialize* ::float
  [obj _type _arena]
  (float obj))

(defmethod serialize* ::double
  [obj _type _arena]
  (double obj))

(defmethod serialize* ::pointer
  [obj type arena]
  (if-not (null? obj)
    (if (sequential? type)
      (let [segment (alloc-instance (second type) arena)]
        (serialize-into obj (second type) segment arena)
        (address-of segment))
      obj)
    null))

(defmethod serialize* ::void
  [_obj _type _arena]
  nil)

(defmulti serialize-into
  "Writes a serialized version of the `obj` to the given `segment`.

  Any new allocations made during the serialization should be tied to the given
  `arena`, except in extenuating circumstances.

  This method should be implemented for any type which does not
  override [[c-layout]].

  For any other type, this will serialize it as [[serialize*]] before writing
  the result value into the `segment`."
  (fn
    #_{:clj-kondo/ignore [:unused-binding]}
    [obj type segment arena]
    (type-dispatch type)))

(defmethod serialize-into :default
  [obj type segment arena]
  (if-some [prim-layout (primitive-type type)]
    (serialize-into (serialize* obj type arena) prim-layout segment arena)
    (throw (ex-info "Attempted to serialize an object to a type that has not been overridden"
                    {:type type
                     :object obj}))))

(defmethod serialize-into ::byte
  [obj _type segment _arena]
  (write-byte segment (byte obj)))

(defmethod serialize-into ::short
  [obj type segment _arena]
  (if (sequential? type)
    (write-short segment 0 (second type) (short obj))
    (write-short segment (short obj))))

(defmethod serialize-into ::int
  [obj type segment _arena]
  (if (sequential? type)
    (write-int segment 0 (second type) (int obj))
    (write-int segment (int obj))))

(defmethod serialize-into ::long
  [obj type segment _arena]
  (if (sequential? type)
    (write-long segment 0 (second type) (long obj))
    (write-long segment (long obj))))

(defmethod serialize-into ::char
  [obj _type segment _arena]
  (write-char segment (char obj)))

(defmethod serialize-into ::float
  [obj type segment _arena]
  (if (sequential? type)
    (write-float segment 0 (second type) (float obj))
    (write-float segment (float obj))))

(defmethod serialize-into ::double
  [obj type segment _arena]
  (if (sequential? type)
    (write-double segment 0 (second type) (double obj))
    (write-double segment (double obj))))

(defmethod serialize-into ::pointer
  [obj type segment arena]
  (write-address
   segment
   (cond-> obj
     (sequential? type) (serialize* type arena))))

(defn serialize
  "Serializes an arbitrary type.

  For types which have a primitive representation, this serializes into that
  representation. For types which do not, it allocates a new segment and
  serializes into that."
  ([obj type] (serialize obj type (auto-arena)))
  ([obj type arena]
   (if (primitive-type type)
     (serialize* obj type arena)
     (let [segment (alloc-instance type arena)]
       (serialize-into obj type segment arena)
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
  (cond-> (read-address segment)
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
      (let [target-type (second type)]
        (deserialize-from
         (.reinterpret ^MemorySegment (read-address addr)
                       ^long (size-of target-type))
         target-type))
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
  (map #(deserialize % type) (slice-segments segment (size-of type))))

;;; Raw composite types
;; TODO(Joshua): Ensure that all the raw values don't have anything happen on
;; serialize in the inlining of [[coffi.ffi/make-serde-wrapper]]

(defmethod c-layout ::raw
  [[_raw type]]
  (c-layout type))

(defmethod serialize-into ::raw
  [obj _type segment _arena]
  (if (instance? MemorySegment obj)
    (copy-segment segment obj)
    obj))

(defmethod deserialize-from ::raw
  [segment _type]
  (if (instance? MemorySegment segment)
    (clone-segment segment)
    segment))

;;; C String type

(defmethod primitive-type ::c-string
  [_type]
  ::pointer)

(defmethod serialize* ::c-string
  [obj _type ^Arena arena]
  (if obj
    (.allocateFrom arena ^String obj)
    null))

(defmethod deserialize* ::c-string
  [addr _type]
  (when-not (null? addr)
    (.getString (.reinterpret ^MemorySegment addr Integer/MAX_VALUE) 0)))

;;; Union types

(defmethod c-layout ::union
  [[_union types & {:as _opts} :as _type]]
  (let [items (map c-layout types)]
    (MemoryLayout/unionLayout
     (into-array MemoryLayout items))))

(defmethod serialize-into ::union
  [obj [_union _types & {:keys [dispatch extract]} :as type] segment arena]
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
     arena)))

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
  [obj [_struct fields] segment arena]
  (loop [offset 0
         fields fields]
    (when (seq fields)
      (let [[field type] (first fields)
            size (size-of type)]
        (serialize-into
         (get obj field) type
         (slice segment offset size) arena)
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
  (MemoryLayout/paddingLayout size))

(defmethod serialize-into ::padding
  [_obj [_padding _size] _segment _arena]
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
  [obj [_array type count] segment arena]
  (dorun
   (map #(serialize-into %1 type %2 arena)
        obj
        (slice-segments (slice segment 0 (* count (size-of type)))
                        (size-of type)))))

(defmethod deserialize-from ::array
  [segment [_array type count]]
  (mapv #(deserialize-from % type)
        (slice-segments (slice segment 0 (* count (size-of type)))
                        (size-of type))))

;;; Enum types

(defmethod primitive-type ::enum
  [[_enum _variants & {:keys [repr]}]]
  (if repr
    (primitive-type repr)
    ::int))

(defn- enum-variants-map
  "Constructs a map from enum variant objects to their native representations.

  Enums are mappings from Clojure objects to numbers, with potential default
  values for each element based on order.

  If `variants` is a map, then every variant has a value provided already (a
  guarantee of maps in Clojure's syntax) and we are done.

  If `variants` is a vector then we assume C-style implicit enum values,
  counting from 0. If an element of `variants` itself is a vector, it must be a
  vector tuple of the variant object to the native representation, with further
  counting continuing from that value."
  [variants]
  (if (map? variants)
    variants
    (first
     (reduce
      (fn [[m next-id] variant]
        (if (vector? variant)
          [(conj m variant) (inc (second variant))]
          [(assoc m variant next-id) (inc next-id)]))
      [{} 0]
      variants))))

(defmethod serialize* ::enum
  [obj [_enum variants & {:keys [repr]}] arena]
  (serialize* ((enum-variants-map variants) obj)
              (or repr ::int)
              arena))

(defmethod deserialize* ::enum
  [obj [_enum variants & {:keys [_repr]}]]
  ((set/map-invert (enum-variants-map variants)) obj))

;;; Flagsets

(defmethod primitive-type ::flagset
  [[_flagset _bits & {:keys [repr]}]]
  (if repr
    (primitive-type repr)
    ::int))

(defmethod serialize* ::flagset
  [obj [_flagset bits & {:keys [repr]}] arena]
  (let [bits-map (enum-variants-map bits)]
    (reduce #(bit-set %1 (get bits-map %2)) (serialize* 0 (or repr ::int) arena) obj)))

(defmethod deserialize* ::flagset
  [obj [_flagset bits & {:keys [repr]}]]
  (let [bits-map (set/map-invert (enum-variants-map bits))]
    (reduce #(if-not (zero? (bit-and 1 (bit-shift-right obj %2)))
               (conj %1 (bits-map %2))
               %1)
            #{}
            (range (* 8 (size-of (or repr ::int)))))))

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
         [obj# _type# arena#]
         (serialize* obj# aliased# arena#))
       (defmethod deserialize* ~new-type
         [obj# _type#]
         (deserialize* obj# aliased#)))
    `(let [aliased# ~aliased-type]
       (defmethod c-layout ~new-type
         [_type#]
         (c-layout aliased#))
       (defmethod serialize-into ~new-type
         [obj# _type# segment# arena#]
         (serialize-into obj# aliased# segment# arena#))
       (defmethod deserialize-from ~new-type
         [segment# _type#]
         (deserialize-from segment# aliased#)))))
(s/fdef defalias
  :args (s/cat :new-type qualified-keyword?
               :aliased-type any?))

(defn- with-c-layout
  "Forces a struct specification to C layout rules.

  This will add padding fields between fields to match C alignment
  requirements."
  [struct-spec]
  (let [aligned-fields
        (loop [offset 0
               aligned-fields []
               fields (nth struct-spec 1)]
          (if (seq fields)
            (let [[[_ type :as field] & fields] fields
                  size (size-of type)
                  align (align-of type)
                  r (rem offset align)]
              (recur (cond-> (+ offset size)
                       (pos? r) (+ (- align r)))
                     (cond-> aligned-fields
                       (pos? r) (conj [:coffi.layout/padding [:coffi.mem/padding (- align r)]])
                       :always (conj field))
                     fields))
            (let [strongest-alignment (reduce max (map (comp align-of second) (nth struct-spec 1)))
                  r (rem offset strongest-alignment)]
              (cond-> aligned-fields
                (pos? r) (conj [:coffi.layout/padding [:coffi.mem/padding (- strongest-alignment r)]])))))]
    (assoc struct-spec 1 aligned-fields)))

(defn- coffitype->typename [in]
  (let [[arr _type n & {:keys [raw?] :as opts}] (if (vector? in) in [:- in])
        arr? (= arr ::array)
        array-types  {::byte   'bytes
                      ::short  'shorts
                      ::int    'ints
                      ::long   'longs
                      ::char   'chars
                      ::float  'floats
                      ::double 'doubles}
        single-types {::byte     'byte
                      ::short    'short
                      ::int      'int
                      ::long     'long
                      ::char     'char
                      ::float    'float
                      ::double   'double
                      ::c-string 'String}]
    (cond (and arr? raw?) (get array-types _type 'objects)
          (and arr?)      `clojure.lang.IPersistentVector
          :default        (get single-types _type (keyword (str *ns*) (str _type))))))

(defn- coffitype->array-fn [_type]
  (get
   {:coffi.mem/byte   `byte-array
    :coffi.mem/short  `short-array
    :coffi.mem/int    `int-array
    :coffi.mem/long   `long-array
    :coffi.mem/char   `char-array
    :coffi.mem/float  `float-array
    :coffi.mem/double `double-array}
   _type
   `object-array))

(defn- coffitype->array-write-fn [_type]
  ({:coffi.mem/byte   `write-bytes
    :coffi.mem/short  `write-shorts
    :coffi.mem/int    `write-ints
    :coffi.mem/long   `write-longs
    :coffi.mem/char   `write-chars
    :coffi.mem/float  `write-floats
    :coffi.mem/double `write-doubles} _type))

(defn- coffitype->array-read-fn [_type]
  ({:coffi.mem/byte   `read-bytes
    :coffi.mem/short  `read-shorts
    :coffi.mem/int    `read-ints
    :coffi.mem/long   `read-longs
    :coffi.mem/char   `read-chars
    :coffi.mem/float  `read-floats
    :coffi.mem/double `read-doubles} _type))

(def array-copy-method :bulk)

(defmulti  generate-deserialize (fn [& xs] (if (vector? (first xs)) (first (first xs)) (first xs))))

(defmethod generate-deserialize :coffi.mem/byte     [_type offset segment-source-form] `(read-byte    ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/short    [_type offset segment-source-form] `(read-short   ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/int      [_type offset segment-source-form] `(read-int     ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/long     [_type offset segment-source-form] `(read-long    ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/char     [_type offset segment-source-form] `(read-char    ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/float    [_type offset segment-source-form] `(read-float   ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/double   [_type offset segment-source-form] `(read-double  ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/pointer  [_type offset segment-source-form] `(read-address ~segment-source-form ~offset))
(defmethod generate-deserialize :coffi.mem/c-string [_type offset segment-source-form] (list `.getString (list `.reinterpret (list `.get (with-meta segment-source-form {:tag 'java.lang.foreign.MemorySegment}) `pointer-layout offset) `Integer/MAX_VALUE) 0))

(defmethod generate-deserialize :coffi.mem/array    [[_ array-type n & {:keys [raw?]}] offset segment-source-form]
  (if (and (= array-copy-method :bulk) (coffitype->array-read-fn array-type))
    (if raw?
      (list (coffitype->array-read-fn array-type) segment-source-form n offset)
      (list `vec (list (coffitype->array-read-fn array-type) segment-source-form n offset)))

    (if (= array-copy-method :loop)
      (let [a (gensym 'array)]
        (concat
         `(let [~a (~(coffitype->array-fn array-type) ~n)])
         [(list `dotimes ['m n]
                (list `aset a 'm (generate-deserialize array-type `(+ ~offset (* ~(size-of array-type) ~'m)) segment-source-form)))]
         [(if raw? a `(vec ~a))]))
      (let [a (gensym 'array)]
       (concat
        `(let [~a (~(coffitype->array-fn array-type) ~n)])
        (map
         #(list `aset a % (generate-deserialize array-type (+ offset (* (size-of array-type) %)) segment-source-form))
         (range n))
        [(if raw? a `(vec ~a))])))))

(defn- typelist [typename fields]
  (->>
   (partition 2 2 (interleave (reductions + 0 (map (comp size-of second) fields)) fields))
   (filter (fn [[_ [_ field-type]]] (not (and (vector? field-type) (= "padding" (name (first field-type)))))))))

(defn register-new-struct-deserialization [typename [_struct fields]]
  (let [typelist (typelist typename fields)]
    (defmethod generate-deserialize typename [_type global-offset segment-source-form]
      (->> typelist
           (map-indexed
            (fn [index [offset [_ field-type]]]
              (generate-deserialize field-type (+ global-offset offset) segment-source-form)))
           (cons (symbol (str (name typename) ".")))))))

(defmulti  generate-serialize (fn [& xs] (if (vector? (first xs)) (first (first xs)) (first xs))))

(defmethod generate-serialize :coffi.mem/byte     [_type source-form offset segment-source-form] `(write-byte    ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/short    [_type source-form offset segment-source-form] `(write-short   ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/int      [_type source-form offset segment-source-form] `(write-int     ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/long     [_type source-form offset segment-source-form] `(write-long    ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/char     [_type source-form offset segment-source-form] `(write-char    ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/float    [_type source-form offset segment-source-form] `(write-float   ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/double   [_type source-form offset segment-source-form] `(write-double  ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/pointer  [_type source-form offset segment-source-form] `(write-pointer ~segment-source-form ~offset ~source-form))
(defmethod generate-serialize :coffi.mem/c-string [_type source-form offset segment-source-form] `(write-address ~segment-source-form ~offset (.allocateFrom (Arena/ofAuto) ~source-form)))

(defmethod generate-serialize :coffi.mem/array   [[_arr member-type length & {:keys [raw?]}] source-form offset segment-source-form]
  (if (and (= array-copy-method :bulk) (coffitype->array-write-fn member-type))
    (if raw?
      (list (coffitype->array-write-fn member-type) segment-source-form length offset source-form)
      (list (coffitype->array-write-fn member-type) segment-source-form length offset (list (coffitype->array-fn member-type) length source-form)))

    (if (= array-copy-method :loop)
      (let [obj (with-meta (gensym 'src-array) {:tag (coffitype->typename [::array member-type length :raw? raw?])})]
        (concat
         (list `let [obj source-form])
         [(list `dotimes ['n length]
            (generate-serialize member-type (list (if raw? `aget `nth) obj 'n) `(+ ~offset (* ~(size-of member-type) ~'n)) segment-source-form)
            )]))

      (let [obj (with-meta (gensym 'src-array) {:tag (coffitype->typename [::array member-type length :raw? raw?])})]
       (concat
        (list `let [obj source-form])
        (map #(generate-serialize member-type (list (if raw? `aget `nth) obj %) (+ offset (* (size-of member-type) %)) segment-source-form)
             (range length)))))))

(defn register-new-struct-serialization [typename [_struct fields]]
  (let [typelist (typelist typename fields)
        fieldnames (filter #(not= "padding" (name %)) (map first fields))]
    (defmethod generate-serialize typename [_type source-form global-offset segment-source-form]
      (->> typelist
           (map-indexed
            (fn [index [offset [_ field-type]]]
              (generate-serialize field-type (list (symbol (str "." (name (nth fieldnames index)))) 'source-obj) (+ global-offset offset) segment-source-form)))
           (concat [`let ['source-obj source-form]])))))

(gen-interface
 :name coffi.mem.IStructImpl :methods
 [[vec_length [] int]
  [vec_assoc [Object Object] clojure.lang.Associative]
  [vec_assocN [int Object] clojure.lang.IPersistentVector]
  [vec_peek [] Object]
  [vec_pop [] clojure.lang.IPersistentVector]
  [vec_nth [int] Object]
  [vec_nth [int Object] Object]
  [vec_cons [Object] clojure.lang.IPersistentCollection]
  [vec_equiv [Object] boolean]
  [vec_empty [] clojure.lang.IPersistentVector]
  [vec_iterator [] java.util.Iterator]
  [vec_forEach [java.util.function.Consumer] void]
  [vec_seq [] clojure.lang.ISeq]
  [vec_rseq [] clojure.lang.ISeq]

  [struct_count [] int]
  [struct_containsKey [Object] boolean]
  [struct_valAt [Object] Object]
  [struct_valAt [Object Object] Object]
  [struct_entryAt [Object] clojure.lang.IMapEntry]
  [nthKey [int] clojure.lang.Keyword]

  [map_assoc [Object Object] clojure.lang.Associative]
  [map_assocEx [Object Object] clojure.lang.IPersistentMap]
  [map_without [Object] clojure.lang.IPersistentMap]
  [map_cons [Object] clojure.lang.IPersistentCollection]
  [map_equiv [Object] boolean]
  [map_empty [] clojure.lang.IPersistentMap]
  [map_iterator [] java.util.Iterator]
  [map_forEach [java.util.function.Consumer] void]
  [map_seq [] clojure.lang.ISeq]
  ;java.util.map fns
  [map_containsValue [Object] boolean]
  [map_entrySet [] java.util.Set]
  [map_get [Object] Object]
  [map_isEmpty [] boolean]
  [map_keySet [] java.util.Set]
  [map_size [] int]
  [map_values [] java.util.Collection]
  [map_forEach [java.util.function.BiConsumer] void]])


(defmacro for-each-fixed-length [n]
  `(defn ~(symbol (str "for-each-fixed-" n)) ~[(with-meta 'offset {:tag int}) (with-meta 'action {:tag 'java.util.function.Consumer}) (with-meta 's {:tag 'coffi.mem.IStructImpl})]
     ~(cons `do (map (fn [i] (list '.accept (with-meta 'action {:tag 'java.util.function.Consumer}) (list '.vec_nth (with-meta 's {:tag 'coffi.mem.IStructImpl}) i))) (range n)))))

(for-each-fixed-length 1)
(for-each-fixed-length 2)
(for-each-fixed-length 3)
(for-each-fixed-length 4)
(for-each-fixed-length 5)
(for-each-fixed-length 6)
(for-each-fixed-length 7)
(for-each-fixed-length 8)
(for-each-fixed-length 9)
(for-each-fixed-length 10)
(for-each-fixed-length 11)
(for-each-fixed-length 12)
(for-each-fixed-length 13)
(for-each-fixed-length 14)
(for-each-fixed-length 15)
(for-each-fixed-length 16)

(deftype struct-vec-iterator [^coffi.mem.IStructImpl struct-obj ^int size ^{:volatile-mutable true :tag int} i]
  java.util.Iterator
  (forEachRemaining [this action]
    (case (- size i)
      1  (for-each-fixed-1  i action struct-obj)
      2  (for-each-fixed-2  i action struct-obj)
      3  (for-each-fixed-3  i action struct-obj)
      4  (for-each-fixed-4  i action struct-obj)
      5  (for-each-fixed-5  i action struct-obj)
      6  (for-each-fixed-6  i action struct-obj)
      7  (for-each-fixed-7  i action struct-obj)
      8  (for-each-fixed-8  i action struct-obj)
      9  (for-each-fixed-9  i action struct-obj)
      10 (for-each-fixed-10 i action struct-obj)
      11 (for-each-fixed-11 i action struct-obj)
      12 (for-each-fixed-12 i action struct-obj)
      13 (for-each-fixed-13 i action struct-obj)
      14 (for-each-fixed-14 i action struct-obj)
      15 (for-each-fixed-15 i action struct-obj)
      16 (for-each-fixed-16 i action struct-obj)
      (loop [index i] (if (< index size) (do (.accept action (.vec_nth struct-obj index)) (recur (inc index))) nil))))
  (hasNext [this] (< i size))
  (next [this] (let [ret (.vec_nth struct-obj i) _ (set! i (unchecked-add-int 1 i))] ret)))

(gen-interface :name coffi.mem.IStruct :methods [[asVec [] clojure.lang.IPersistentVector] [asMap [] clojure.lang.IPersistentMap]])

(deftype StructVecSeq [^clojure.lang.IPersistentVector v ^int i]
  clojure.lang.ISeq clojure.lang.Indexed clojure.lang.Sequential
  (first [this] (.nth v i))
  (next  [this] (if (< i (dec (.count v))) (StructVecSeq. v (inc i)) nil))
  (more  [this] (if (< i (dec (.count v))) (StructVecSeq. v (inc i)) '()))
  (cons  [this o] (clojure.lang.Cons. o this))
  (count [this] (- (.count v) i))
  (empty [this] nil)
  (equiv [this o] (= (subvec v i) o))
  (nth   [this j] (.nth v (+ i j)))
  (nth   [this j o] (.nth v (+ i j) o))
  (seq   [this] this))

(deftype StructMapSeq [^coffi.mem.IStructImpl s ^int i]
  clojure.lang.ISeq clojure.lang.Indexed clojure.lang.Sequential
  (first [this] (clojure.lang.MapEntry/create (.nthKey s i) (.vec_nth s i)))
  (next  [this] (if (< i (dec (.struct_count s))) (StructMapSeq. s (inc i)) nil))
  (more  [this] (if (< i (dec (.struct_count s))) (StructMapSeq. s (inc i)) '()))
  (cons  [this o] (clojure.lang.Cons. o this))
  (count [this] (- (.struct_count s) i))
  (empty [this] nil)
  (equiv [this o] (if (not= (count o) (.struct_count s)) false (loop [os (seq o) index i] (if (< index (- (.struct_count s) i)) (if (= [(.nthKey s index) (.vec_nth s index)] (first os)) (recur (next os) (inc index)) false) true))))
  (nth   [this j] (clojure.lang.MapEntry/create (.nthKey s (+ i j)) (.vec_nth s (+ i j))))
  (nth   [this j o] (if (< (+ i j) (.struct_count s)) (clojure.lang.MapEntry/create (.nthKey s (+ i j)) (.vec_nth s (+ i j))) o))
  (seq   [this] this))

(deftype VecWrap [^coffi.mem.IStructImpl org]
  coffi.mem.IStruct clojure.lang.IPersistentVector Iterable
  (length      [this]     (.vec_length org))
  (assoc       [this k v] (.vec_assoc org k v))
  (assocN      [this i v] (.vec_assocN org i v))
  (peek        [this]     (.vec_peek org))
  (pop         [this]     (.vec_pop org))
  (nth         [this i]   (.vec_nth org i))
  (nth         [this i o] (.vec_nth org i o))
  (cons        [this o]   (.vec_cons org o))
  (equiv       [this o]   (.vec_equiv org o))
  (empty       [this]     (.vec_empty org))
  (iterator    [this]     (.vec_iterator org))
  (forEach     [this c]   (.vec_forEach org c))
  (seq         [this]     (StructVecSeq. this 0))
  (rseq        [this]     (.vec_rseq org))
  (count       [this]     (.struct_count org))
  (containsKey [this k]   (.struct_containsKey org k))
  (valAt       [this k]   (.struct_valAt org k))
  (valAt       [this k o] (.struct_valAt org k o))
  (entryAt     [this k]   (.struct_entryAt org k))
  (asMap       [this]     org)
  (asVec       [this]     this))

(deftype MapWrap [^coffi.mem.IStructImpl org]
  coffi.mem.IStruct clojure.lang.IPersistentMap clojure.lang.MapEquivalence java.util.Map
  (cons        [this o]   (.map_cons org o))
  (equiv       [this o]   (.map_equiv org o))
  (empty       [this]     (.map_empty org))
  (iterator    [this]     (.map_iterator org))
  (^void forEach [this ^java.util.function.Consumer c] (.map_forEach org c))
  (^void forEach [this ^java.util.function.BiConsumer c] (.map_forEach org c))
  (seq         [this]     (StructMapSeq. org 0))
  (assoc       [this k v] (.map_assoc org k v))
  (count       [this]     (.struct_count org))
  (containsKey [this k]   (.struct_containsKey org k))
  (valAt       [this k]   (.struct_valAt org k))
  (valAt       [this k o] (.struct_valAt org k o))
  (entryAt     [this k]   (.struct_entryAt org k))
  (assocEx     [this k v] (.map_assocEx org k v))
  (without     [this k]   (.map_without org k))
  ;java.util.map implementations
  (containsValue [this k] (.map_containsValue org k))
  (entrySet      [this]   (.map_entrySet org))
  (get           [this k] (.map_get org k))
  (isEmpty       [this]   false)
  (keySet        [this]   (.map_keySet org))
  (size          [this]   (.map_size org))
  (values        [this]   (.map_values org))
  ;conversion methods
  (asMap       [this]     this)
  (asVec       [this]     org)
  )

(defn as-vec [^coffi.mem.IStruct struct] (.asVec struct))
(defn as-map [^coffi.mem.IStruct struct] (.asMap struct))


(defn- generate-struct-type [typename typed-member-symbols]
  (let [members (map (comp keyword str) typed-member-symbols)
        as-vec (vec (map (comp symbol name) members))
        as-map (into {} (map (fn [m] [m (symbol (name m))]) members))]
    (letfn [(vec-length    [] (list 'length      ['this]           (count members)))
            (vec-assoc     [] (list 'assoc       ['this 'i 'value] (list `assoc as-vec 'i 'value)))
            (vec-assocN    [] (list 'assocN      ['this 'i 'value] (list `assoc 'i as-vec 'value)))
            (vec-peek      [] (list 'peek        ['this]           (first as-vec)))
            (vec-pop       [] (list 'pop         ['this]           (vec (rest as-vec))))
            (vec-nth       [] (list 'nth         ['this 'i]        (concat [`case 'i] (interleave (range) as-vec))))
            (vec-nth-2     [] (list 'nth         ['this 'i 'o]     (concat [`case 'i] (interleave (range) as-vec) ['o])))
            (vec-cons      [] (list 'cons        ['this 'o]        (vec (cons 'o as-vec))))
            (vec-equiv     [] (list 'equiv       ['this 'o]        (list `= as-vec 'o)))
            (vec-empty     [] (list 'empty       ['this]           []))
            (vec-iterator  [] (list 'iterator    ['this]           (list `struct-vec-iterator. 'this (count members) 0)))
            (vec-foreach   [] (concat ['forEach  ['this 'action]]  (partition 2 (interleave (repeat 'action) as-vec))))
            (vec-seq       [] (list 'seq         ['this]           (list `StructVecSeq. 'this 0)))
            (vec-rseq      [] (list 'rseq        ['this]           (list `seq (vec (reverse as-vec)))))

            (s-count       [] (list 'count       ['this]           (count members)))
            (s-containsKey [] (list 'containsKey ['this 'k]        (list `if (list `number? 'k) (list `and (list `>= 'k 0) (list `< 'k (count members)) true) (list `case 'k (seq members) true false))))
            (s-valAt       [] (list 'valAt       ['this 'k]        (concat [`case 'k] (interleave (range) as-vec) (interleave members as-vec) [nil])))
            (s-valAt-2     [] (list 'valAt       ['this 'k 'o]     (concat [`case 'k] (interleave (range) as-vec) (interleave members as-vec) ['o])))
            (s-entryAt     [] (list 'entryAt     ['this 'k]        (list `let ['val-or-nil (concat [`case 'k] (interleave (range) as-vec) (interleave members as-vec) [nil])] (list `if 'val-or-nil (list `clojure.lang.MapEntry/create 'k 'val-or-nil) nil))))

            (map-assoc     [] (list 'assoc       ['this 'i 'value] (list `assoc as-map 'i 'value)))
            (map-assocEx   [] (list 'assocEx     ['this 'i 'value] (list `if (list (set members) 'i) (list `throw (list `Exception. "key already exists")) (assoc as-map 'i 'value))))
            (map-without   [] (list 'without     ['this 'k]        (list `dissoc as-map (list `if (list `number? 'k) (list (vec members) 'k) 'k))))
            (map-cons      [] (list 'cons        ['this 'o]        `(if (instance? clojure.lang.MapEntry ~'o) ~(conj as-map [`(.key ~(with-meta 'o {:tag 'clojure.lang.MapEntry})) `(.val ~(with-meta 'o {:tag 'clojure.lang.MapEntry}))]) (if (instance? clojure.lang.IPersistentVector ~'o) ~(conj as-map [`(.nth ~(with-meta 'o {:tag 'clojure.lang.IPersistentVector}) 0) `(.nth ~(with-meta 'o {:tag 'clojure.lang.IPersistentVector}) 1)]) (.cons ~(with-meta 'o {:tag 'clojure.lang.IPersistentMap}) ~as-map)))))
            (map-equiv     [] (list 'equiv       ['this 'o]        (list `= as-map 'o)))
            (map-empty     [] (list 'empty       ['this]           {}))
            (map-iterator  [] (list 'iterator    ['this]           (list '.iterator as-map)))
            (map-foreachConsumer   [] (concat [(with-meta 'forEach {:tag 'void})  ['this (with-meta 'action {:tag 'java.util.function.Consumer}) ]]  (partition 2 (interleave (repeat 'action) as-map))))
            (map-foreachBiConsumer   [] (concat [(with-meta 'forEach {:tag 'void})  ['this (with-meta 'action {:tag 'java.util.function.BiConsumer})]]  (partition 3 (flatten (interleave (repeat 'action) (seq as-map))))))
            (map-seq       [] (list 'seq         ['this]           (list `StructMapSeq. 'this 0)))
            (invoke1       [] (list 'invoke      ['this 'arg1]       (concat [`case 'arg1] (interleave (range) as-vec) (interleave members as-vec) [nil])))
            (invoke2       [] (list 'invoke      ['this 'arg1 'arg2] (concat [`case 'arg1] (interleave (range) as-vec) (interleave members as-vec) ['arg2])))
            (applyTo       [] (list 'applyTo      ['this 'arglist] (concat [`case (list `first 'arglist)] (interleave (range) as-vec) (interleave members as-vec) [(list `if (list `.next 'arglist) (list `.first (list `.next 'arglist)) nil)])))
            ;structimpl utility function
            (s-nth-key     [] (list 'nthKey     ['this 'i]        (concat [`case 'i] (interleave (range) members))))
            ;java.util.Map implementations
            (map-contains-value [] (list 'containsValue ['this 'val] (list `some (set as-vec) 'val)))
            (map-entrySet       [] (list 'entrySet ['this] (set (map (fn [[k v]] (list `clojure.lang.MapEntry/create k v)) (partition 2 (interleave members as-vec))))))
            (map-get            [] (cons 'get (rest (s-valAt))))
            (map-isEmpty        [] (list 'isEmpty ['this] false))
            (map-keySet         [] (list 'keySet ['this] (set members)))
            (map-size           [] (list 'size ['this] (count members)))
            (map-values         [] (list 'values ['this] as-vec))

            (map-methods   [] [(map-without) (map-cons) (map-equiv) (map-empty) (map-iterator) (map-foreachConsumer) #_(map-foreachBiConsumer) (map-seq) (map-assoc) (map-assocEx) (map-contains-value) (map-entrySet) (map-get) (map-isEmpty) (map-keySet) (map-size) (map-values)])
            (vec-methods   [] [(vec-length) (vec-assoc) (vec-assocN) (vec-peek) (vec-pop) (vec-nth) (vec-nth-2) (vec-cons) (vec-equiv) (vec-empty) (vec-iterator) (vec-foreach) (vec-seq) (vec-rseq)])
            (struct-methods [] [(s-count) (s-containsKey) (s-valAt) (s-valAt-2) (s-entryAt)])
            (prefix-methods [prefix ms] (map (fn [[method-name & tail]] (cons (with-meta (symbol (str prefix method-name)) (meta method-name)) tail)) ms))
            (impl-methods [] (concat (prefix-methods "map_" (map-methods)) (prefix-methods "vec_" (vec-methods)) (prefix-methods "struct_" (struct-methods))))]
      (concat
       [`deftype (symbol (name typename)) (vec typed-member-symbols) `coffi.mem.IStruct `coffi.mem.IStructImpl `clojure.lang.IPersistentMap `clojure.lang.MapEquivalence `java.util.Map `clojure.lang.IFn]
       (struct-methods)
       (map-methods)
       (impl-methods)
       [(s-nth-key)
        (invoke1)
        (invoke2)
        (list 'asMap ['this] 'this)
        (list 'asVec ['this] (list `VecWrap. 'this))]))))

(defmacro defstruct
  "Defines a struct type. all members need to be supplied in pairs of `coffi-type member-name`.

  This creates needed serialization and deserialization implementations for the new type.

  The typenames have to be coffi typenames, such as `:coffi.mem/int` or `[:coffi.mem/array :coffi.mem/byte 3]`.
  Arrays are wrapped with vectors by default. If you want to use raw java arrays the array type has to be supplied with the option `:raw? true`, for example like this `[:coffi.mem/array :coffi.mem/byte 3 :raw? true]`
  "
  {:style/indent [:defn]}
  [typename members]
  (let [invalid-typenames (filter #(try (c-layout (first %)) nil (catch Exception e (first %))) (partition 2 members))]
    (cond
      (odd? (count members)) (throw (Exception. "uneven amount of members supplied. members have to be typed and are required to be supplied in the form of `typename member-name`. the typename has to be coffi typename, like `:coffi.mem/int` or `[:coffi.mem/array :coffi.mem/byte 3]`"))
      (seq invalid-typenames) (throw (Exception. (str "invalid typename/s " (print-str invalid-typenames) ". typename has to be coffi typename, like `:coffi.mem/int` or `[:coffi.mem/array :coffi.mem/byte 3]`. The type/s you referenced also might not be defined. In case of a custom type, ensure that you use the correctly namespaced keyword to refer to it.")))
      :else
      (let [coffi-typename (keyword (str *ns*) (str typename))
            typed-symbols (->>
                           members
                           (partition 2 2)
                           (map (fn [[_type sym]] (with-meta sym {:tag (coffitype->typename _type)})))
                           (vec))
            struct-layout-raw [::struct
                               (->>
                                members
                                (partition 2 2)
                                (map vec)
                                (map #(update % 1 keyword))
                                (map reverse)
                                (map vec)
                                (vec))]
            struct-layout (with-c-layout struct-layout-raw)
            segment-form (with-meta 'segment {:tag 'java.lang.foreign.MemorySegment})]
        (if (resolve typename) (ns-unmap *ns* typename))
        (register-new-struct-deserialization coffi-typename struct-layout)
        (register-new-struct-serialization   coffi-typename struct-layout)
        `(do
           ~(generate-struct-type typename typed-symbols)
           (defmethod c-layout ~coffi-typename [~'_] (c-layout ~struct-layout))
           (defmethod deserialize-from ~coffi-typename ~[segment-form '_type]
             ~(generate-deserialize coffi-typename 0 segment-form))
           (defmethod serialize-into ~coffi-typename ~[(with-meta 'source-obj {:tag typename}) '_type segment-form '_]
             ~(generate-serialize coffi-typename (with-meta 'source-obj {:tag typename}) 0 segment-form))
           (defmethod clojure.pprint/simple-dispatch ~typename [~'obj] (clojure.pprint/simple-dispatch (into {} ~'obj)))
           (defmethod clojure.core/print-method ~typename [~'obj ~'writer] (print-simple (into {} ~'obj) ~'writer)))))))


