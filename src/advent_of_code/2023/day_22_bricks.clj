(ns advent-of-code.2023.day-22-bricks
  (:require [advent-of-code.common :as common]))

;; --- Parsing ---

(defn range-incl [a b] (range (min a b) (inc (max a b))))

(defn points-along [[x1 y1 z1 x2 y2 z2]]
  (for [x (range-incl x1 x2) y (range-incl y1 y2) z (range-incl z1 z2)] [x y z]))

(defn z-coord [[_x _y z]] z)
(defn xy-coords [[x y _z]] [x y])
(defn vec+ [& pt's] (apply map + pt's))

(defn parse [input]
  (->> input
       common/parse-longs
       (partition 6)
       ;; We sort the cubes within the brick by z-coord so that we can (1) sort
       ;; the bricks from low to high here, and (2) have the highest cube last
       ;; when we merge them into the `tops` map below.
       (map #(sort-by z-coord (points-along %)))
       (sort-by #(z-coord (first %)))))

;; --- Working with bricks ---

(defn get-stack
  "Lower each brick until it rests on something. Return a `stack`, a map of id
   to two lists: :rests-on and :supports."
  [bricks]
  (loop [[brick & brick's] bricks id 0 tops {} stack {}]
    (if-not brick
      (assoc stack :count id)
      (let [[z lands-on] (->> (into #{} (map xy-coords brick))
                              (map #(tops % [0 nil]))
                              (group-by first)
                              (sort-by first >)
                              first)
            lands-on (keep second lands-on)
            orig-z (z-coord (first brick))
            new-loc's (map #(vec+ % [0 0 (- (inc z) orig-z)]) brick)]
        (recur brick's
               (inc id)
               (into tops (map (fn [[x y z]] [[x y] [z id]]) new-loc's))
               (reduce (fn [acc b]
                         (-> acc
                             (update-in [id :rests-on] (fnil conj #{}) b)
                             (update-in [b :supports] (fnil conj #{}) id)))
                       stack
                       lands-on))))))

(defn will-fall
  "If we remove a brick, return which other bricks would then fall."
  [stack]
  (fn _will-fall
    ([id] (_will-fall id #{id}))
    ([id goners]
     (->> (get-in stack [id :supports])
          (map (juxt identity #(get-in stack [% :rests-on])))
          (keep (fn [[b supports]] (when (empty? (remove goners supports)) b)))))))

(defn remove-brick
  "Cascade removing a brick, the next bricks that would fall, then the next, and
   so on."
  [stack]
  (let [fall-er (will-fall stack)]
    (fn [id]
      (loop [queue (common/queue id) n 0 goners #{}]
        (let [[b queue] ((juxt peek pop) queue)]
          (if-not b
            (dec n)   ; Number disintegrated, not including the first one.
            (let [goners (conj goners b)]
              (recur (into queue (fall-er b goners))
                     (inc n)
                     goners))))))))

(comment
  (def stack (get-stack (parse (slurp "input/2023/22-sample.txt"))))
  (def stack (get-stack (parse (slurp "input/2023/22-bricks.txt"))))

  ;; year 2023 day 22 puzzle 1
  (->> (range (:count stack))
       (map (will-fall stack))
       (filter empty?)
       count)
  ;; => 473

  ;; year 2023 day 22 puzzle 2
  (transduce (comp (map (remove-brick stack))) + (range (:count stack)))
  ;; => 61045
  )
