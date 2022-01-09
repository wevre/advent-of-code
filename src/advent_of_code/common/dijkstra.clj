(ns advent-of-code.common.dijkstra
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defprotocol ICost
  (lowest-cost [this base other])
  (total-cost [this]))

(defrecord Cost [cost heur]
  ICost
  (lowest-cost [this base other]
               (let [new-cost (+ (.cost base) (.cost other))]
                 (if (and this (< (.cost this) new-cost))
                   this
                   (->Cost new-cost (.heur other)))))
  (total-cost [_] (+ cost heur)))

(defprotocol IState
  (node [this])
  (cost [this])
  (next-states [this]))

(defn update-costs [base]
  (fn [pmap state]
    (update pmap (node state) lowest-cost base (cost state))))

(defn find-lowest-cost [start end?]
  (loop [costs (priority-map-keyfn total-cost (node start) (cost start))
         visited #{}]
    (let [[node cost] (peek costs)]
      (if (end? node)
        cost
        (let [states (remove #(visited (node %)) (next-states node))]
          (recur (reduce (update-costs cost) (pop costs) states)
                 (conj visited node)))))))
