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
  (let [even's (state-at-n garden #{start} 130)
        E (count even's) O (count (state-at-n garden even's 1))
        N (dec 202300)   ; (/ (- 26501365 65) 131) => 202300
        tips (for [loc [[0 65] [65 0] [65 130] [130 65]]]
               (count (state-at-n garden #{loc} 130)))
        edges (for [loc [[0 0] [130 0] [0 130] [130 130]]]
                (let [outer (state-at-n garden #{loc} 64)]
                  (+ (* N (count (state-at-n garden outer 131)))
                     (* (inc N) (count outer)))))]
    (+ O                           ; central garden
       (* O (inc N) (dec N))       ; odd full gardens
       (* E (* (inc N) (inc N)))   ; even full gardens
       (reduce + tips)
       (reduce + edges)))
  ;; => 623540829615589
  )

;; For drawing out the gardens.
(defn draw-locs [garden]
  (let [[rows cols] (:size garden)]
    (fn [loc's]
      (println (str (apply str (repeat 65 \-)) \+ (apply str (repeat 65 \-))))
      (doseq [r (range rows)]
        (print (if (= r 65) \+ \|))
        (doseq [c (range cols)]
          (let [g (garden [r c])]
            (print (cond (= g \S) \S (loc's [r c]) \O :else g))))
        (println (if (= r 65) \+ \|)))
      (println (str (apply str (repeat 65 \-)) \+ (apply str (repeat 65 \-)))))))
