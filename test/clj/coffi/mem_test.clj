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
  (let [v1 (TestType. 5 10 15)
        v2 (TestType. 6 11 16)]
    (t/are [x y] (= x (y v1))
     5  :a
     10 :b
     15 :c
     5  (fn [v] (v :a))
     10 (fn [v] (v :b))
     15 (fn [v] (v :c))
     5  #(get % :a)
     10 #(get % :b)
     15 #(get % :c)
     20 #(get % :d 20)
     nil #(get % :d)
     [:a :b :c] keys
     [5 10 15]  vals
     {:a 5 :c 15} #(dissoc % :b)
     {:a 5 :b 10 :c 0} #(assoc % :c 0)
     {:a 5 :b 10 :c 15 :d 20} #(assoc % :d 20)
     [[:a 5] [:b 10] [:c 15]] seq
     {:a 5 :b 10 :c 15 :d 20} #(merge % {:d 20})
     {:a [5 6] :b [10 11] :c [15 16]} #(merge-with vector % {:a 6 :b 11 :c 16})
     {:a [5 6] :b [10 11] :c [15 16]} #(merge-with vector % v2)
     [:a 5] #(find % :a)
     nil #(find % :d)
     {:a 5 :b 10 :c 15} identity
     v1 identity
     v1 (fn [s] {:a 5 :b 10 :c 15}))))

(t/deftest can-serialize-struct-type
  (t/is
   (instance? MemorySegment (mem/serialize (TestType. 5 10 15) ::TestType))))

(t/deftest can-deserialize-struct-type
  (t/is
   (= {:a 5 :b 10 :c 15}
      (mem/deserialize (mem/serialize (TestType. 5 10 15) ::TestType) ::TestType))))

(mem/defstruct NestedTestType [::mem/int x ::mem/byte y ::TestType z])

(t/deftest can-instantiated-nested-structs
  (t/is
   (= {:x 5 :y 6 :z {:a 5 :b 10 :c 15}}
      (NestedTestType. 5 6 (TestType. 5 10 15)))))

(t/deftest can-define-structs-with-array-members
  (t/is
   (eval
    `(mem/defstruct ~'ArrayTestType [::mem/int ~'x ::mem/byte ~'y [::mem/array ::mem/int 4 :raw? true] ~'z]))))

(mem/defstruct ArrayTestType [::mem/int x ::mem/byte y [::mem/array ::mem/int 4 :raw? true] z])

(t/deftest can-instantiated-array-member-structs
  (t/are [x y z] (z x (y (ArrayTestType. 5 6 (int-array [1 2 3 4]))))
    {:x 5 :y 6} #(dissoc % :z) =
    (int-array [1 2 3 4]) :z java.util.Arrays/equals))

(t/deftest can-serialize-array-struct
  (t/is
   (= [5 6 1 2 3 4]
      (vec (filter #(not= 0 %) (vec (.toArray (mem/serialize (ArrayTestType. 5 6 (int-array [1 2 3 4])) ::ArrayTestType) mem/byte-layout)))))))

(t/deftest can-serialize-deserialize-array-struct
  (t/is
   (java.util.Arrays/equals
    (int-array [1 2 3 4])
    (.z (mem/deserialize (mem/serialize (ArrayTestType. 5 6 (int-array [1 2 3 4])) ::ArrayTestType) ::ArrayTestType)))))

(mem/defstruct ComplexTestType [[::mem/array ::ArrayTestType 4 :raw? true] x ::mem/byte y [::mem/array ::mem/int 4 :raw? true] z ::NestedTestType w])

(t/deftest can-serialize-deserialize-complex-struct-type
  (t/is
   (let [x (object-array (map #(ArrayTestType. % % (int-array (range 4))) (range 4)))
         y 12
         z (int-array (range 4))
         w (NestedTestType. 5 6 (TestType. 5 10 15))]
     (->
      (ComplexTestType. x y z w)
      (mem/serialize ::ComplexTestType)
      (mem/deserialize ::ComplexTestType)))))

(mem/defstruct ComplexTestTypeWrapped [[::mem/array ::ArrayTestType 4] x ::mem/byte y [::mem/array ::mem/int 4] z ::NestedTestType w])

(t/deftest can-serialize-deserialize-complex-wrapped-struct-type
  (t/is
   (let [x (vec (map #(ArrayTestType. % % (int-array (range 4))) (range 4)))
         y 12
         z (vec (range 4))
         w (NestedTestType. 5 6 (TestType. 5 10 15))]
     (->
      (ComplexTestTypeWrapped. x y z w)
      (mem/serialize ::ComplexTestTypeWrapped)
      (mem/deserialize ::ComplexTestTypeWrapped)))))

