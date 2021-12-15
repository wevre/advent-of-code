(ns advent-of-code.2021.day-15-transient
  (:require [clojure.string :as str]))

;; --- Day 15: Chiton ---
;; https://adventofcode.com/2021/day/15

;; the map entry will be:
;; [x y] {:risk 5 :dist #Inf :prev [x y]}

;;TODO: [] use transients (not faster! but maybe my impl is not good)
;;      [] could use vector of vectors instead of map

(def start [0 0])
(def end [99 99])
(def dim 100)

(defn assoc-in! [m [k & ks] v]
  (if ks
    (assoc! m k (assoc-in! (get m k) ks v))
    (assoc! m k v)))

(defn update!
  ([m k f]
   (assoc! m k (f (get m k))))
  ([m k f x]
   (assoc! m k (f (get m k) x)))
  ([m k f x y]
   (assoc! m k (f (get m k) x y))))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map-indexed vector)
       (reduce (fn [acc [r l]]
                 (reduce (fn [acc [c v]]
                           (assoc! acc [r c] (-> {:risk (Character/digit v 10)
                                                  :dist ##Inf
                                                          :visited? false}
                                                 transient)))
                         acc
                         (map-indexed vector l)))
               (transient {}))
       (#(assoc-in! % [start :dist] 0))
       )
  )

(defn neighbors [[r c]]
  (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ r dr) (+ c dc)]))

(defn smallest-unvisited [chitons]
  (reduce (fn [loc1 loc2]
            (if (< (:dist (chitons loc1)) (:dist (chitons loc2))) loc1 loc2))
          (for [r (range dim) c (range dim) :when (and (< (:dist (chitons [r c])) ##Inf)
                                                       (not (:visited? (chitons [r c]))))]
                                                        [r c]))
          #_
  (->> chitons persistent!
       (reduce (fn [e1 e2] (if (< (:dist (val e1)) (:dist (val e2))) e1 e2)))
       first))

(comment
  (-> (parse-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581") (smallest-unvisited)))


(defn unvisited-neighbors [node chitons]
  (->> node neighbors (select-keys chitons) (remove #(:visited? (val %))) keys))

(defn update-node [node-map dist]
  (let [test-dist (+ dist (:risk node-map))]
    (if (< test-dist (:dist node-map))
      (assoc! node-map :dist test-dist)
      node-map)))

(defn lowest-risk [chitons]
  (loop [chitons chitons]
    (if (:visited? (chitons end))
      (:dist (chitons end))
      (let [node (smallest-unvisited chitons)
            neighbors (unvisited-neighbors node chitons)]
        (recur (reduce (fn [acc loc]
                         (update! acc loc update-node (:dist (chitons node))))
                       (assoc-in! chitons [node :visited?] true)
                       neighbors))))))

(comment

  (with-redefs [dim 10
                end [9 9]]

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
