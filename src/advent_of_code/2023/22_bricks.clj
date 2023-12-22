(ns advent-of-code.2023.22-bricks
  (:require [advent-of-code.common :as common]))

;; --- Parsing ---

(defn range-incl [a b] (range (min a b) (inc (max a b))))

(defn points-along [[x1 y1 z1] [x2 y2 z2]]
  (for [x (range-incl x1 x2) y (range-incl y1 y2) z (range-incl z1 z2)] [x y z]))

(defn brick<- [[i [a b]]]
  (let [cubes (sort-by #(nth % 2) (points-along a b))
        [_ _ min-z] (first cubes)]
    {:id i :cubes cubes :min-z min-z}))

(defn parse [input]
  (->> input
       common/parse-longs
       (partition 3)
       (partition 2)
       (map-indexed vector)
       (map brick<-)))

;; --- Letting bricks come to rest ---

(defn drop-bricks [bricks]
  (loop [[{:as b :keys [cubes min-z id]} & b's] (sort-by :min-z bricks) tops {}
         supports {} rests-on {}]
    (if-not b
      {:supports supports :rests-on rests-on}
      (let [footprint (keep (fn [[x y z]] (when (= z min-z) [x y])) cubes)
            [z lands-on] (->> footprint
                              (map #(tops % [0 :ground]))
                              (group-by first)
                              (sort-by first >)
                              first)
            new-pos (map #(map + [0 0 (- (inc z) min-z)] %) cubes)]
        (recur b's
               (into tops (map (fn [[x y z]] [[x y] [z id]]) new-pos))
               (reduce (fn [acc [_ b]] (update acc b (fnil conj #{}) id))
                       supports
                       lands-on)
               (reduce (fn [acc [_ b]] (update acc id (fnil conj #{}) b))
                       rests-on
                       lands-on))))))

(comment
  (def bricks (parse (slurp "input/2023/22-sample.txt")))
  (def bricks (parse (slurp "input/2023/22-bricks.txt")))

  ;; year 2023 day 22 puzzle 1
  (let [{:keys [supports rests-on]} (drop-bricks bricks)]
    (reduce (fn [n {:keys [id]}]
              (if (every? #(< 1 (count %)) (map rests-on (supports id)))
                (inc n)
                n))
            0
            bricks))
  ;; => 473
  )

;; --- Chain reaction ---

(defn remove-brick [supports rests-on id]
  (loop [queue (common/queue id) n 0 goners #{}]
    (let [[b queue] ((juxt peek pop) queue)]
      (if-not b
        n
        (let [goners (conj goners b)]
          (recur (into queue (filter #(empty? (remove goners (rests-on %))) (supports b)))
                 (inc n)
                 goners))))))

(comment
  ;; year 2023 day 22 puzzle 2
  (let [{:keys [supports rests-on]} (drop-bricks bricks)]
    (->> bricks
         (map :id)
         (map #(remove-brick supports rests-on %))
         (map dec)
         (reduce +)))
  ;; => 61045

  )
