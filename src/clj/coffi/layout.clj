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
                  align (mem/align-of type)
                  r (rem offset align)]
              (recur (cond-> (+ offset size)
                       (pos? r) (+ (- align r)))
                     (cond-> aligned-fields
                       (pos? r) (conj [::padding [::mem/padding (- align r)]])
                       :always (conj field))
                     fields))
            (let [strongest-alignment (reduce max (map (comp mem/align-of second) (nth struct-spec 1)))
                  r (rem offset strongest-alignment)]
              (cond-> aligned-fields
                (pos? r) (conj [::padding [::mem/padding (- strongest-alignment r)]])))))]
    (assoc struct-spec 1 aligned-fields)))
