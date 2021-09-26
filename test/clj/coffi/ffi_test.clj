(ns coffi.ffi-test
  (:require
   [clojure.test :as t]
   [coffi.mem :as mem]
   [coffi.ffi :as ffi]))

(ffi/load-library "target/ffi_test.so")

(t/deftest can-load-symbols
  (t/is (not (nil? (ffi/find-symbol "add_numbers")))))

(t/deftest can-call-primitive-fns
  (t/is (= 5 ((ffi/cfn "add_numbers" [::mem/int ::mem/int] ::mem/int) 2 3))))

(mem/defalias ::point
  [::mem/struct
   [[:x ::mem/float]
    [:y ::mem/float]]])

(t/deftest can-call-with-structs
  (t/is (= {:x 2.0 :y 2.0}
           ((ffi/cfn "add_points" [::point ::point] ::point) {:x 1 :y 2} {:x 1 :y 0}))))

(t/deftest can-call-deserialized-fn-pointers
  (t/is (= "Alternate string"
           (((ffi/cfn "get_downcall" [::mem/int] [::ffi/fn [] ::mem/c-string])
             1)))))

(t/deftest can-make-upcall
  (t/is (= ((ffi/cfn "upcall_test" [[::ffi/fn [] ::mem/c-string]] ::mem/c-string)
            (fn [] "hello"))
           "hello")))
