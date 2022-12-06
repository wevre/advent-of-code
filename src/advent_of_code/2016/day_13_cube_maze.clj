(ns advent-of-code.2016.day-13-cube-maze
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.dijkstra :as dijkstra]
            [clojure.java.math :as math]))

(def ^:dynamic *input* 1358)

(def ^:dynamic *target* [31 39])

(defn binary [x y]
  (-> (+ (* x x) (* 3 x) (* 2 x y) y (* y y) *input*) (Long/toString 2)))

(defn wall? [val] (->> val (filter #{\1}) count odd?))

(defrecord State [pos]
  dijkstra/IState
  (state-key [_this] pos)

  (cost [_this]
    (let [[x y] pos [tx ty] *target*]
      (+ (math/abs (- tx x)) (math/abs (- ty y)))))

  (next-states [_this]
    (let [[x y] pos]
      (for [[dx dy] [[-1 0] [1 0] [0 1] [0 -1]]
            :let [x (+ x dx) y (+ y dy)]
            :when (and (<= 0 x) (<= 0 y)
                       (not (wall? (binary x y))))]
        (->State [x y]))))

  (end? [_this] (= pos *target*)))

(defn find-path [start]
  (when-let [{:keys [node]} (dijkstra/find-lowest-cost (->State start))]
    (loop [i 0 node node]
      (if node
        (recur (inc i) (dijkstra/prev-node node))
        (dec i)))))

(comment
  ;; puzzle 1
  (find-path [1 1])   ; => 96

  ;; puzzle 2
  (->>
   (for [x (common/range-x 52) y (common/range-x 52)
         :when (not (wall? (binary x y)))]
     (binding [*target* [x y]] (find-path [1 1])))
   (remove nil?)
   (filter #(<= % 50))
   count)   ; => 141

  (binding [*input* 10 *target* [7 4]]
    (find-path [1 1]))

  (binding [*input* 10]
    (let [wid 10 hei 7]
      (doseq [line
              (->> (for [y (range hei) x (range wid)]
                     (if (wall? (binary x y)) \# \.))
                   (partition wid)
                   (map #(apply str %)))]
        (println line))))
  )