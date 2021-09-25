(ns ^:no-doc coffi.hooks
  (:require
   [clj-kondo.hooks-api :as api]))

(defn defcfn
  [{:keys [node]}]
  (let [[] (rest (:children node))]
    ;; TODO(Joshua): Add an implementation of this macro's hook
    ))
