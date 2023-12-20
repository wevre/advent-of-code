(ns advent-of-code.common.astar
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defprotocol INode
  (node-key [this] "Unique key for the node for the priority queue.")
  (g-score [this] "Cost/weight/dist associated with this node; cost from start to node.")
  (h-score [this] "Heuristic for this node. Estimated cost from node to goal.")
  (nigh's [this] "Nodes reached from this node.")
  (?end [this] "True if this node is end state."))

(defn- f-score [node] (+ (g-score node) (h-score node)))

(defn- ?update [costs node]
  (let [curr (costs (node-key node))]
    (or (not curr) (< (g-score node) (g-score curr)))))

;; TODO: maybe let's get this to work without a record, just a map of the
;; various functions we need.

(defn lowest-cost [node]
  (loop [costs (priority-map-keyfn f-score (node-key node) node) visited {}]
    (when-let [[[node-key node] costs] ((juxt peek pop) costs)]
      (if (?end node)
        {:node node :visited visited}
        (let [nighs (filter #(?update costs %) (nigh's node))]
          (print (map (fn [n] [(node-key n) n]) nighs))
          (recur (into costs (map (fn [n] [(node-key n) n]) nighs))
                 (into visited (map #(vector (node-key %) node) nighs))))))))
