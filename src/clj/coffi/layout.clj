(ns coffi.layout
  "Functions for adjusting the layout of structs."
  (:require
   [coffi.mem :as mem]))

(def with-c-layout
  @#'mem/with-c-layout)
(alter-meta! #'with-c-layout #(merge (-> (meta #'mem/with-c-layout)
                                         (dissoc :private))
                                     %))
