(ns advent-of-code.2022.day-12-elevations
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.dijkstra :as dijkstra]))

;; 2022-12-11 10:55
;;    Solutions
;; 2022-12-11 11:09
;;    Decided to not put locations, size, start and end all in same map.
;; 2022-12-11 11:21
;;    Don't need to check if [x y] is valid coord, lookup will be nil.
;; 2022-12-11 11:33
;;    Don't need to destructure x and y everywhere.
;; 2022-12-12 00:14
;;    Was on way to bed and had brilliant idea. Instead of brute-force searching
;;    from every possible trailhead, just reverse the search (and do it only one
;;    time) from the original end point. Less cumbersome code, and faster, too.

(defrecord State [pos end-fn height-fn heights]
  dijkstra/IState
  (state-key [_this] pos)

  (cost [_this] 1)

  (next-states [_this]
    (let [curr-height (get heights pos)]
      (for [∆ [[-1 0] [1 0] [0 1] [0 -1]]
            :let [next-pos (mapv + pos ∆)
                  to-height (get heights next-pos)]
            :when (and to-height (height-fn curr-height to-height))]
        (->State next-pos end-fn height-fn heights))))

  (end? [_this] (end-fn pos)))

(defn parse
  "Return two maps, `[info heights]`, with integer heights keyed by `[x y]`
   position and `info` contains useful entries: `:start`, `:end`.
   "
  [input]
  (let [{:keys [locmap]} (common/locmap<- input)]
    (reduce (fn [[info heights] [k v]]
              (cond
                (= v \S) [(assoc info :start k) (assoc heights k 0)]
                (= v \E) [(assoc info :end k) (assoc heights k 25)]
                :else [info (assoc heights k (- (int v) (int \a)))]))
            [{} {}]
            locmap)))

(defn solve [end-fn height-fn start heights]
  (->> (->State start end-fn height-fn heights)
       dijkstra/find-lowest-cost
       :node
       (iterate dijkstra/prev-node)
       (take-while identity)
       count
       dec))

(comment
  ;; sample puzzle
  (let [[info heights] (parse "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")]
    (solve #(= % (:end info)) #(<= (- %2 %1) 1) (:start info) heights))   ; => 31

  ;; puzzle 1
  (let [[info heights] (parse (slurp "input/2022/12-elevations.txt"))]
    (solve #(= % (:end info)) #(<= (- %2 %1) 1) (:start info) heights))   ; => 339

  ;; puzzle 2
  (let [[info heights] (parse (slurp "input/2022/12-elevations.txt"))]
    (solve #(= 0 (get heights %)) #(>= (- %2 %1) -1) (:end info) heights))   ; => 332
  )
