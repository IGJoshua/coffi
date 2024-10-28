(ns coffi.ffi-test
  (:require
   [clojure.test :as t]
   [coffi.ffi :as ffi]
   [coffi.layout :as layout]
   [coffi.mem :as mem]))

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
            (fn [] "hello from clojure from c from clojure"))
           "hello from clojure from c from clojure")))

(t/deftest can-make-upcall2
  (t/is (= ((ffi/cfn "upcall_test2" [[::ffi/fn [] ::mem/int]] ::mem/int)
            (fn [] 5))
           5)))

(t/deftest can-make-upcall-int-fn-string-ret
  (t/is (= ((ffi/cfn "upcall_test_int_fn_string_ret" [[::ffi/fn [] ::mem/int]] ::mem/c-string)
            (fn [] 2))
           "co'oi prenu")))

(mem/defalias ::alignment-test
  (layout/with-c-layout
    [::mem/struct
     [[:a ::mem/char]
      [:x ::mem/double]
      [:y ::mem/float]]]))

(t/deftest padding-matches
  (t/is (= (dissoc ((ffi/cfn "get_struct" [] ::alignment-test)) ::layout/padding)
           {:a \x
            :x 3.14
            :y 42.0})))

(t/deftest static-variables-are-mutable
  (ffi/freset! (ffi/static-variable "counter" ::mem/int) 1)
  (t/is (= ((ffi/cfn "get_string1" [] ::mem/c-string))
           "Goodbye friend.")))

(t/deftest can-call-with-trailing-string-arg
  (t/is
   (= (try ((ffi/cfn "test_call_with_trailing_string_arg"
                     [::mem/int ::mem/int ::mem/c-string]
                     ::mem/void)
            1 2 "third arg")
           :ok
           (catch Throwable _t
             :err))
      :ok)))


(mem/defstruct Point [::mem/float x ::mem/float y])

(t/deftest can-call-with-defstruct
  (t/is (= {:x 2.0 :y 2.0}
           ((ffi/cfn "add_points" [::Point ::Point] ::Point) (Point. 1 2) (Point. 1 0)))))

(mem/defstruct AlignmentTest [::mem/char a ::mem/double x ::mem/float y])

(t/deftest padding-matches-defstruct
  (t/is (= ((ffi/cfn "get_struct" [] ::AlignmentTest))
           {:a \x
            :x 3.14
            :y 42.0})))

(mem/defstruct ComplexType [::Point x ::mem/byte y [::mem/array ::mem/int 4] z ::mem/c-string w])

(t/deftest can-call-with-complex-defstruct
  (t/are [x y] (= x (y ((ffi/cfn "complexTypeTest" [::ComplexType] ::ComplexType)
                        (ComplexType. (Point. 2 3) 4 (int-array [5 6 7 8]) "hello from clojure"))))
    {:x {:x 3.0 :y 4.0} :y 3 :w "hello from c"} #(dissoc % :z)
    [5 6 7 8] (comp vec :z)))



