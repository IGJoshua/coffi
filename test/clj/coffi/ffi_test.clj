(ns coffi.ffi-test
  (:require
   [clojure.test :as t]
   [coffi.ffi :as ffi]
   [coffi.layout :as layout]
   [coffi.mem :as mem]))

(ffi/load-library "target/ffi_test.so")

(t/deftest can-load-symbols
  (t/is (not (nil? (ffi/find-symbol "add_numbers")))))

(t/deftest can-fetch-constant
  (t/is (= 42 (ffi/const "c" ::mem/int))))

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

(ffi/defvar freed? "freed" ::mem/int)

(def get-variable-length-array* (ffi/make-downcall "get_variable_length_array" [::mem/pointer] ::mem/int))
(def free-variable-length-array* (ffi/make-downcall "free_variable_length_array" [::mem/pointer] ::mem/void))

(t/deftest get-variable-length-array
  (let [floats
        (with-open [stack (mem/confined-arena)]
          (let [out-floats (mem/alloc mem/pointer-size stack)
                num-floats (get-variable-length-array* out-floats)
                floats-addr (mem/read-address out-floats)
                floats-slice (mem/reinterpret floats-addr (unchecked-multiply-int mem/float-size num-floats))]
            (try
              (loop [floats (transient [])
                     index 0]
                (if (>= index num-floats)
                  (persistent! floats)
                  (recur (conj! floats (mem/read-float floats-slice (unchecked-multiply-int index mem/float-size)))
                         (unchecked-inc-int index))))
              (finally
                (free-variable-length-array* floats-addr)))))]
    (t/is (not (zero? @freed?)))
    (t/is (= floats (mapv #(* (float 1.5) %) (range (count floats)))))))
