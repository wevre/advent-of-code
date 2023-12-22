(ns advent-of-code.2023.day-21-steps
  (:require [advent-of-code.common2 :as common2]))

(defn steps [garden]
  (let [[rows cols] (:size garden)]
    (fn [loc's]
      (reduce (fn [acc loc]
                (into acc (for [∆ [[-1 0] [1 0] [0 -1] [0 1]]
                                :let [[r c :as n] (mapv + loc ∆)]
                                :when (and (not= \# (garden n))
                                           (<= 0 r (dec rows))
                                           (<= 0 c (dec cols)))]
                            n)))
              #{}
              loc's))))

(defn state-at-n [garden loc's N]
  (->> (iterate (steps garden) loc's) (drop N) first))

(comment
  (do
    (def garden (into {} (common2/locmap<- identity :?size true) (slurp "input/2023/21-sample.txt")))
    (def start (->> garden (keep #(when (= \S (second %)) (first %))) first)))

  (do
    (def garden (into {} (common2/locmap<- identity :?size true) (slurp "input/2023/21-garden.txt")))
    (def start (->> garden (keep #(when (= \S (second %)) (first %))) first))
    (def size (:size garden)))

  ;; year 2023 day 21 puzzle 1
  (count (state-at-n garden #{start} 64))
  ;; => 3746

  ;; year 2023 day 21 puzzle 2
  (time
   (let [[rows _cols] (:size garden)   ; => [131 131]
         even's (state-at-n garden #{start} (dec rows))
         E (count even's) O (count (state-at-n garden even's 1))
         N (dec (/ (- 26501365 65) rows))   ; => 202299
         mid (/ (dec rows) 2)
         verts (for [loc [[0 mid] [mid 0] [mid (dec rows)] [(dec rows) mid]]]
                (count (state-at-n garden #{loc} (dec rows))))
         edges (for [loc [[0 0] [(dec rows) 0] [0 (dec rows)] [(dec rows) (dec rows)]]]
                 (let [outer (state-at-n garden #{loc} (dec mid))
                       inner (state-at-n garden outer rows)]
                   (+ (* N (count inner)) (* (inc N) (count outer)))))]
     (+ O                       ; central garden
        (* O (inc N) (dec N))   ; odd full gardens
        (* E (inc N) (inc N))   ; even full gardens
        (reduce + verts)
        (reduce + edges))))
  ;; => 623540829615589 (about 21s)
  )

;; For drawing/investigating the garden.
(defn draw-locs [garden]
  (let [[rows cols] (:size garden)
        mid (/ (dec rows) 2)]
    (fn [loc's]
      (println (str \+ (apply str (repeat mid \-)) \+ (apply str (repeat mid \-)) \+))
      (doseq [r (range rows)]
        (print (if (= r mid) \+ \|))
        (doseq [c (range cols)]
          (let [g (garden [r c])]
            (print (cond (= g \S) \S (loc's [r c]) \O (= r mid) \- (= c mid) \| :else g))))
        (println (if (= r mid) \+ \|)))
      (println (str \+ (apply str (repeat mid \-)) \+ (apply str (repeat mid \-)) \+)))))

(comment
  ((draw-locs garden) (state-at-n garden #{[0 0]} 65)))
