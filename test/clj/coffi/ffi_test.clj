(ns coffi.ffi-test
  (:require
   [clojure.test :as t]
   [coffi.ffi :as sut]))

(sut/load-library "target/ffi_test.so")

(t/deftest can-load-symbols
  (t/is (not (nil? (sut/find-symbol "add_points")))))
