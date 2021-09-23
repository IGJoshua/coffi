(ns coffi.ffi-test
  (:require
   [clojure.test :as t]
   [coffi.ffi :as sut]))

(sut/load-library "target/ffi_test.so")

(t/deftest can-load-symbols
  (t/is (not (nil? (sut/find-symbol "add_numbers")))))

(t/deftest can-call-primitive-fns
  (t/is (= 5 ((sut/cfn "add_numbers" [::sut/int ::sut/int] ::sut/int) 2 3))))

(sut/defstruct ::point
  :x ::sut/float
  :y ::sut/float)

(t/deftest can-call-with-structs
  (t/is (= {:x 2.0 :y 2.0}
           ((sut/cfn "add_points" [::point ::point] ::point) {:x 1 :y 2} {:x 1 :y 0}))))

(t/deftest can-call-deserialized-fn-pointers
  (t/is (= "Alternate string"
           (((sut/cfn "get_downcall" [::sut/int] [::sut/fn [] ::sut/c-string :wrap-serde? true])
             1)))))
