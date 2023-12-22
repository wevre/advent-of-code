(ns advent-of-code.2023.day-22-bricks
  (:require [advent-of-code.common :as common]))

;; --- Parsing ---

(defn range-incl [a b] (range (min a b) (inc (max a b))))

(defn points-along [[x1 y1 z1] [x2 y2 z2]]
  (for [x (range-incl x1 x2) y (range-incl y1 y2) z (range-incl z1 z2)] [x y z]))

(defn brick<- [[i [a b]]]
  (let [cubes (sort-by #(nth % 2) (points-along a b))
        [_ _ min-z] (first cubes)
        cubes (map (fn [[x y z]] [x y (- z min-z)]) cubes)
        footprint (keep (fn [[x y z]] (when (zero? z) [x y])) cubes)]
    {:id i :cubes cubes :z-loc min-z :footprint footprint}))

(defn parse [input]
  (->> input
       common/parse-longs
       (partition 3)
       (partition 2)
       (map-indexed vector)
       (map brick<-)
       (sort-by :z-loc)))

;; --- Working with bricks ---

(defn get-stack [bricks]
  (loop [[{:as b :keys [cubes id footprint]} & b's] bricks tops {} stack {}]
    (if-not b
      stack
      (let [[z lands-on] (->> footprint
                              (map #(tops % [0 nil]))
                              (group-by first)
                              (sort-by first >)
                              first)
            lands-on (keep second lands-on)
            new-loc's (map #(map + [0 0 (inc z)] %) cubes)]
        (recur b's
               (into tops (map (fn [[x y z]] [[x y] [z id]]) new-loc's))
               (reduce (fn [acc b]
                         (-> acc
                             (update-in [id :rests-on] (fnil conj #{}) b)
                             (update-in [b :supports] (fnil conj #{}) id)))
                       stack
                       lands-on))))))

(defn other-supporters [stack id]
  (->> (get-in stack [id :supports])
       (map (juxt identity #(get-in stack [% :rests-on])))))

(defn remove-brick [stack]
  (fn [id]
    (loop [queue (common/queue id) n 0 goners #{}]
      (let [[b queue] ((juxt peek pop) queue)]
        (if-not b
          (dec n)
          (let [goners (conj goners b)]
            (recur (into queue (->> (other-supporters stack b)
                                    (keep (fn [[a b]]
                                            (when (empty? (remove goners b)) a)))))
                   (inc n)
                   goners)))))))

(comment
  (do
    (def bricks (parse (slurp "input/2023/22-sample.txt")))
    (def stack (get-stack bricks)))
  (do
    (def bricks (parse (slurp "input/2023/22-bricks.txt")))
    (def stack (get-stack bricks)))

  ;; year 2023 day 22 puzzle 1
  (reduce (fn [n {:keys [id]}]
            (if (every? #(< 1 %) (->> (other-supporters stack id) (map second) (map count)))
              (inc n)
              n))
          0
          bricks)
  ;; => 473

  ;; year 2023 day 22 puzzle 2
  (transduce (comp (map :id) (map (remove-brick stack))) + bricks)
  ;; => 61045
  )
