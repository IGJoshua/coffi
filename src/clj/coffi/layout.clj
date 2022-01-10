(ns coffi.layout
  "Functions for adjusting the layout of structs."
  (:require
   [coffi.mem :as mem]))

(defn with-c-layout
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
                  size (mem/size-of type)
                  r (rem offset (mem/align-of type))]
              (recur (+ offset r size)
                     (cond-> aligned-fields
                       (pos? r) (conj [:padding [::mem/padding r]])
                       :always (conj field))
                     fields))
            (let [strongest-alignment (mem/align-of struct-spec)
                  r (rem offset strongest-alignment)]
              (cond-> aligned-fields
                (pos? r) (conj [:padding [::mem/padding r]])))))]
    (assoc struct-spec 1 aligned-fields)))
