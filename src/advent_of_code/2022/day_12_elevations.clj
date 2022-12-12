(ns advent-of-code.2022.day-12-elevations
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.dijkstra :as dijkstra]
            [clojure.java.math :as math]))

;; 2022-12-11 10:55
;;    Solutions
;; 2022-12-11 11:09
;;    Decided to not put locations, size, start and end all in same map.

(defrecord State [pos info heights]
  dijkstra/IState
  (state-key [_this] pos)

  (cost [_this]
    (let [[x y] pos [tx ty] (:end info)]
      (+ (math/abs (- tx x)) (math/abs (- ty y)))))

  (next-states [_this]
    (let [[x y] pos
          curr-height (get heights pos)
          [cols rows] (:size info)]
      (for [[∆x ∆y] [[-1 0] [1 0] [0 1] [0 -1]]
            :let [x (+ x ∆x) y (+ y ∆y)
                  to-height (get heights [x y])]
            :when (and (<= 0 x) (< x cols) (<= 0 y) (< y rows)
                       (<= (- to-height curr-height) 1))]
        (->State [x y] info heights))))

  (end? [_this] (= pos (:end info))))

(defn parse
  "Return two maps, `[info heights]` with integer heights keyed by `[x y]`
   position and `info` contains useful entries: `:start`, `:end`, and `:size`.
   "
  [input]
  (let [{:keys [locmap size]} (common/locmap<- input)]
    (reduce (fn [[info heights] [k v]]
              (cond
                (= v \S) [(assoc info :start k) (assoc heights k 0)]
                (= v \E) [(assoc info :end k) (assoc heights k 25)]
                :else [info (assoc heights k (- (int v) (int \a)))]))
            [{:size size} {}]
            locmap)))

(defn solve [[{:keys [start] :as info} heights]]
  (->> (->State start info heights)
       dijkstra/find-lowest-cost
       :node
       (iterate dijkstra/prev-node)
       (take-while identity)
       count
       dec))

(comment
  ;; sample puzzle
  (-> (parse "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")
      solve)  ; => 31

  ;; puzzle 1
  (solve (parse (slurp "input/2022/12-elevations.txt")))   ; => 339

  ;; puzzle 2
  (let [[info heights] (parse (slurp "input/2022/12-elevations.txt"))
        trailheads (keep (fn [[k v]] (when (zero? v) k)) heights)]
    (->> trailheads
         (map (fn [start] [(assoc info :start start) heights]))
         (map solve)
         (filter #(< 0 %))
         sort
         first))   ; => 332
  )
