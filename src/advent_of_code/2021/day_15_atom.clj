(ns advent-of-code.2021.day-15-atom
  (:require [advent-of-code.common :refer [locmap<-digits]]))

;; --- Day 15: Chiton ---
;; https://adventofcode.com/2021/day/15

;; the map entry will be:
;; [x y] {:risk 5 :dist #Inf :prev [x y]}

;;TODO: [] use transients (not faster! but maybe my impl is not good)
;;      [] we could keep a set of visited, instead of in the map of each node.
;;      (doens't affect much)
;;      [] could use a big vector of vectors
;;      [] could try a map (or vector) of atoms. (doesn't affect much)

(def start [0 0])
(def end [99 99])

(defn parse-input [s]
  (let [chitons (locmap<-digits s #(atom {:risk % :dist ##Inf}))]
    (swap! (chitons start) assoc :dist 0)
    chitons))

(defn neighbors [[r c]]
  (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ r dr) (+ c dc)]))

(defn smallest-unvisited [visited chitons]
  (->> chitons (remove #(visited (key %))) (sort-by #(:dist @(val %))) ffirst))

(defn unvisited-neighbors [node visited chitons]
  (->> node neighbors (select-keys chitons) (remove #(visited (key %))) keys))

(defn update-node [node-map dist]
  (let [test-dist (+ dist (:risk node-map))]
    (if (< test-dist (:dist node-map))
      (assoc node-map :dist test-dist)
      node-map)))

(defn lowest-risk [chitons]
  (loop [visited #{}]
    (if (visited end)
      (:dist @(chitons end))
      (let [node (smallest-unvisited visited chitons)
            neighbors (unvisited-neighbors node visited chitons)]
        (doseq [loc neighbors]
          (swap! (chitons loc) update-node (:dist @(chitons node))))
        (recur (conj visited node))))))

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
   (lowest-risk (parse-input (slurp "input/2021/15-chitons.txt")))))

(comment
  (-> (sorted-set 8 5 3 1)
      (conj 2)
      first)
  )