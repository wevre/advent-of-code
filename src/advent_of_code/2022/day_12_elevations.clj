(ns advent-of-code.2022.day-12-elevations
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.dijkstra :as dijkstra]
            [clojure.java.math :as math]))

(defrecord State [pos heights]
  dijkstra/IState
  (state-key [_this] pos)

  (cost [_this]
    (let [[x y] pos [tx ty] (:end heights)]
      (+ (math/abs (- tx x)) (math/abs (- ty y)))))

  (next-states [_this]
    (let [[x y] pos
          curr-height (get heights pos)
          [cols rows] (:size heights)]
      (for [[∆x ∆y] [[-1 0] [1 0] [0 1] [0 -1]]
            :let [x (+ x ∆x) y (+ y ∆y)
                  to-height (get heights [x y])]
            :when (and (<= 0 x) (< x cols) (<= 0 y) (< y rows)
                       (<= (- to-height curr-height) 1))]
        (->State [x y] heights))))

  (end? [_this] (= pos (:end heights))))

(defn parse
  "Return a locmap with [x y] keys and height values, but also a few extra,
   useful entries: `:start`, `:end`, and `:size`.
   "
  [input]
  (let [{:keys [locmap size]} (common/locmap<- input)]
    (reduce (fn [m [k v]]
              (cond
                (= v \S) (-> m (assoc :start k k 0))
                (= v \E) (-> m (assoc :end k k 25))
                :else (assoc m k (- (int v) (int \a)))))
            {:size size}
            locmap)))

(defn solve [{:keys [start] :as heights}]
  (->> (->State start heights)
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
  (let [heights (parse (slurp "input/2022/12-elevations.txt"))
        trailheads (->> (dissoc heights :start :end :size)
                        (keep (fn [[k v]] (when (zero? v) k))))]
    (->> trailheads
         (map #(assoc heights :start %))
         (map solve)
         (filter #(< 0 %))
         sort
         first))   ; => 332
  )
