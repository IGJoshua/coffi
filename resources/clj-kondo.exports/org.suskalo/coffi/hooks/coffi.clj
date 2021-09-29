(ns ^:no-doc hooks.coffi
  (:require
   [clj-kondo.hooks-api :as api]))

(defn validate-type
  [node]
  (when-not (or (qualified-keyword? (api/sexpr node))
                (and (api/vector-node? node)
                     (qualified-keyword? (api/sexpr (first (:children node))))))
    (api/reg-finding!
     {:row (:row (meta node))
      :col (:col (meta node))
      :message "A type must be a qualified keyword or a vector with one as the first element."
      :type :coffi.ffi/invalid-syntax})))

(defn defcfn
  [{:keys [node]}]
  (try
    (let [[var-name-node & more] (rest (:children node))
          [docstring-node & more] (if (and (api/string-node? (first more))
                                           (not (api/vector-node? (second more))))
                                    more
                                    (cons nil more))
          [attr-map-node & more] (if (api/map-node? (first more))
                                   more
                                   (cons nil more))
          [symbol-node native-arglist-node return-type-node & more] more
          _ (when-not (or (and (api/token-node? symbol-node)
                               (simple-symbol? (api/sexpr symbol-node)))
                          (api/string-node? symbol-node))
              (api/reg-finding! {:row (:row (meta symbol-node))
                                 :col (:col (meta symbol-node))
                                 :message "Native symbol must be a string or symbol."
                                 :type :coffi.ffi/invalid-syntax}))
          _ (run! validate-type (cons return-type-node (:children native-arglist-node)))
          wrapper-nodes (when (seq more)
                          {:native-fn (first more)
                           :fn-tail (rest more)})
          _ (when (and (:native-fn wrapper-nodes)
                       (empty? (:fn-tail wrapper-nodes)))
              (api/reg-finding!
               {:row (:row (meta node))
                :col (:col (meta node))
                :message "A defcfn with a native-fn must have a function body."
                :type :coffi.ffi/invalid-syntax}))
          arglist-vec (api/vector-node
                       (mapv api/token-node
                             (repeatedly (count (:children native-arglist-node))
                                         #(gensym "arg"))))
          fn-body (if wrapper-nodes
                    (:fn-tail wrapper-nodes)
                    (list
                     arglist-vec
                     arglist-vec))
          defn-node (api/list-node
                     (list*
                      (api/token-node 'defn)
                      var-name-node
                      (concat
                       (filter some? [docstring-node attr-map-node])
                       fn-body)))
          let-node (api/list-node
                    (list
                     (api/token-node 'let)
                     (api/vector-node
                      (cond->> nil
                        wrapper-nodes (concat [(:native-fn wrapper-nodes)
                                               (api/list-node
                                                (list
                                                 (api/token-node 'fn)
                                                 arglist-vec
                                                 arglist-vec))])
                        :always vec))
                     defn-node))]
      {:node let-node})
    (catch Exception _
      (api/reg-finding!
       {:row (:row (meta node))
        :col (:col (meta node))
        :message "Invalid syntax"
        :type :coffi.ffi/invalid-syntax}))))
