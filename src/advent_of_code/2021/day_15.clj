(ns advent-of-code.2021.day-15
  (:require [advent-of-code.common :refer [locmap<-digits]]))

;; --- Day 15: Chiton ---
;; https://adventofcode.com/2021/day/15

;; the map entry will be:
;; [x y] {:risk 5 :dist #Inf :prev [x y]}

;;TODO: [] use transients (not faster! but maybe my impl is not good)
;;      [] could use a big vector of vectors
;;      [] could try a map (or vector) of atoms.
;; I think vector is better than map, because we have to represent the entire
;; grid anyway, and then we don't have to keep track of a location.
;;      so we need a function to lookup in (lookup chiton loc) that will get the
;;      correct value out of the vector.

(def start [0 0])
(def end [99 99])

(defn parse-input [s]
  (-> (locmap<-digits s #(hash-map :risk % :dist ##Inf :visited? false))
      (assoc-in [start :dist] 0)))

(defn neighbors [[r c]]
  (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ r dr) (+ c dc)]))

(defn smallest-unvisited [chitons]
  (->> chitons (remove #(:visited? (val %))) (sort-by #(:dist (val %))) ffirst))

(defn unvisited-neighbors [node chitons]
  (->> node neighbors (select-keys chitons) (remove #(:visited? (val %))) keys))

(defn update-node [node-map dist]
  (let [test-dist (+ dist (:risk node-map))]
    (if (< test-dist (:dist node-map))
      (assoc node-map :dist test-dist)
      node-map)))

(defn lowest-risk [chitons]
  (loop [chitons chitons]
    (if (:visited? (chitons end))
      (:dist (chitons end))
      (let [node (smallest-unvisited chitons)
            neighbors (unvisited-neighbors node chitons)]
        (recur (reduce (fn [acc loc]
                         (update acc loc update-node (:dist (chitons node))))
                       (assoc-in chitons [node :visited?] true)
                       neighbors))))))

(comment

  (with-redefs [end [9 9]]
    (time
     (lowest-risk (parse-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"))))

  ;; puzzle 1
  (time
   (lowest-risk (parse-input (slurp "input/2021/15-chitons.txt"))))
  ;;=> 811 "Elapsed time: 25791.095624 msecs"
  )
