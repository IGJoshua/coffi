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

(t/deftest can-define-struct
  (t/is
   (eval
    `(mem/defstruct ~'TestType [::mem/int ~'a ::mem/byte ~'b]))))

(mem/defstruct TestType [::mem/int a ::mem/byte b ::mem/short c])

(t/deftest can-initialize-struct
  (t/is (TestType. 5 10 15)))

(t/deftest can-use-common-map-functions
  (t/are [x y] (= x (y (TestType. 5 10 15)))
    5  :a
    10 :b
    15 :c
    5  #(% :a)
    10 #(% :b)
    15 #(% :c)
    5  #(get :a)
    10 #(get :b)
    15 #(get :c)
    20 #(get :d 20)
    nil #(get :d)
    [:a :b :c] keys
    [5 10 15]  vals
    {:a 5 :c 15} #(dissoc % :b)
    {:a 5 :b 10 :c 0} #(assoc % :c 0)
    {:a 5 :b 10 :c 15 :d 20} #(assoc % :d 20)
    [[:a 5] [:b 10] [:c 15]] seq
    {:a 5 :b 10 :c 15 :d 20} #(merge % {:d 20})
    {:a [5 6] :b [10 11] :c [15 16]} #(merge-with vector % {:a 6 :b 11 :c 16})
    {:a [5 6] :b [10 11] :c [15 16]} #(merge-with vector % (TestType. 6 11 16))
    [:a 5] #(find % :a)
    nil #(find % :d)
    {:a 5 :b 10 :c 15} identity
    (TestType. 5 10 15) identity
    (TestType. 5 10 15) (fn [s] {:a 5 :b 10 :c 15})))

(t/deftest can-serialize-struct-type
  (t/is
   (instance? MemorySegment (mem/serialize (TestType. 5 10 15) ::TestType))))

(t/deftest can-deserialize-struct-type
  (t/is
   (= {:a 5 :b 10 :c 15}
      (mem/deserialize (mem/serialize (TestType. 5 10 15) ::TestType) ::TestType))))


(t/deftest can-define-nested-structs
  (t/is
   (eval
    `(mem/defstruct ~'NestedTestType [::mem/int ~'x ::mem/byte ~'y ::TestType ~'z]))))

(t/deftest can-instantiated-nested-structs
  (t/is
   (= {:x 5 :y 6 :z {:a 5 :b 10 :c 15}}
      (NestedTestType. 5 6 (TestType. 5 10 15)))))

(t/deftest can-define-structs-with-array-members
  (t/is
   (eval
    `(mem/defstruct ~'ArrayTestType [::mem/int ~'x ::mem/byte ~'y [::mem/array ::mem/int 4] ~'z]))))

(t/deftest can-instantiated-array-member-structs
  (t/are [x y z] (z x (y (ArrayTestType. 5 6 (int-array [1 2 3 4]))))
      {:x 5 :y 6} #(dissoc % :z) =
      (int-array [1 2 3 4]) :z java.util.Arrays/equals))


