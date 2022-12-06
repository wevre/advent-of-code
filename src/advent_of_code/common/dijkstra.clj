(ns advent-of-code.common.dijkstra
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defprotocol IState
  (state-key [this] "Unique identifier for state")
  (cost [this] "Cost associated with state")
  (next-states [this] "States reached directly from current state")
  (end? [this] "State is end state?"))

(defprotocol INode
  (state [this] "State stored at this node.")
  (path-cost [this] "Cost to reach this node.")
  (prev-node [this]))

(defrecord Node [state path-cost prev-node]
  INode
  (state [_this] state)
  (path-cost [_this] path-cost)
  (prev-node [_this] prev-node))

(defn update-costs [node]
  (let [base-cost (+ (path-cost node) (cost (state node)))]
    (fn [costs state]
      (update costs (state-key state)
              (fn [curr-node]
                (if (or (not curr-node) (< base-cost (path-cost curr-node)))
                  (->Node state base-cost node)
                  curr-node))))))

(defn find-lowest-cost [start]
  (let [cost-fn (fn [node] (+ (path-cost node) (cost (state node))))]
    (loop [costs (priority-map-keyfn cost-fn (state-key start) (->Node start 0 nil))
           visited #{}]
      (when-let [[s-key node] (peek costs)]
        (let [state (state node)]
          (if (end? state)
            {:node node :costs costs :visited visited}
            (let [states (remove #(visited (state-key %)) (next-states state))]
              (recur (reduce (update-costs node) (pop costs) states)
                     (conj visited s-key)))))))))
