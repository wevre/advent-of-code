(ns advent-of-code.2021.day-15
  (:require [advent-of-code.common :refer [locmap<-digits]]))

;; --- Day 15: Chiton ---
;; https://adventofcode.com/2021/day/15

;; the map entry will be:
;; [x y] {:risk 5 :dist #Inf :prev [x y]}

(def start [0 0])
(def end [99 99])

(defn parse-input [s]
  (-> (locmap<-digits s #(hash-map :risk % :dist ##Inf :prev nil :visited? false))
      (assoc-in [start :dist] 0)))

(defn neighbors [[r c]]
  (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ r dr) (+ c dc)]))

(defn smallest-unvisited [chitons]
  (->> chitons (remove #(:visited? (val %))) (sort-by #(:dist (val %))) ffirst))

(defn unvisited-neighbors [node chitons]
  (->> node neighbors (select-keys chitons) (remove #(:visited? (val %))) keys))

(defn update-node [node-map curr dist]
  (let [test-dist (+ dist (:risk node-map))]
    (if (< test-dist (:dist node-map))
      (assoc node-map :dist test-dist :prev curr)
      node-map)))

(defn lowest-risk [chitons]
  (loop [chitons chitons]
    (if (:visited? (chitons end))
      (:dist (chitons end))
      (let [node (smallest-unvisited chitons)
            neighbors (unvisited-neighbors node chitons)]
        (recur (reduce (fn [acc loc]
                         (update acc loc update-node node (:dist (chitons node))))
                       (assoc-in chitons [node :visited?] true)
                       neighbors))))))

(comment
  ;; puzzle 1
  (time
   (lowest-risk (parse-input (slurp "input/2021/15-chitons.txt"))))
  )
