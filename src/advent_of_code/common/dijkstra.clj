(ns advent-of-code.common.dijkstra
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

;; 2022-12-12
;;    Use a simple map in lieu of INode protocol and Node datatype. This was
;;    (mostly) internal to this namespace, but consumers did use `prev-node` so
;;    added function `path` to provide that.

(defprotocol IState
  (state-key [this] "Unique identifier for state")
  (cost [this] "Cost associated with state")
  (next-states [this] "States reached directly from current state")
  (end? [this] "State is end state?"))

(defn- path-cost [node]
  (+ (::cost node) (cost (::state node))))

(defn update-costs [node]
  (let [base-cost (path-cost node)]
    (fn [costs state]
      (update costs (state-key state)
              (fn [curr-node]
                (if (or (not curr-node) (< base-cost (::cost curr-node)))
                  {::state state ::cost base-cost ::prev node}
                  curr-node))))))

(defn lowest-cost
  "Return map of target node and (ostensibly for debugging purposes) `costs`
   priority map and `visited` set. Within `node`, walking `::prev` yields
   lowest-cost path."
  [start]
  (loop [costs (priority-map-keyfn path-cost
                                   (state-key start)
                                   {::state start ::cost 0 ::prev nil})
         visited #{}]
    (when-let [[s-key node] (peek costs)]
      (let [state (::state node)]
        (if (end? state)
          {:node node :costs costs :visited visited}
          (let [states (remove #(visited (state-key %)) (next-states state))]
            (recur (reduce (update-costs node) (pop costs) states)
                   (conj visited s-key))))))))

(defn path
  "Returns path from final node back to (and including) start node."
  [node]
  (->> node
       (iterate ::prev)
       (map #(dissoc % ::prev))
       (take-while identity)))
