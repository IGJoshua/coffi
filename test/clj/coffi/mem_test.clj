(ns coffi.mem-test
  (:require
   [clojure.test :as t]
   [coffi.ffi :as ffi]
   [coffi.layout :as layout]
   [coffi.mem :as mem])
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
   (java.nio ByteOrder)))

(ffi/load-library "target/ffi_test.so")

(t/deftest can-serialize-string
  (t/is
   (instance? MemorySegment (mem/serialize "this is a string" ::mem/c-string))))


