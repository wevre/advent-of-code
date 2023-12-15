(ns advent-of-code.2023.day-14-v2
  (:require [advent-of-code.common2 :as common2]))

(defn scoot-col [rows]
  (fn [rocks c]
    (loop [[loc & loc's] (for [r (range rows)] [r c]) stop [-1 c] rocks rocks]
      (if-not loc
        rocks
        (let [entry (get rocks loc)]
          (case entry
            \# (recur loc's loc rocks)
            \O (let [r (mapv + [1 0] stop)]
                 (recur loc's r (-> rocks
                                    (dissoc loc)
                                    (assoc r entry))))
            (recur loc's stop rocks)))))))

(defn scoot [[rows cols]]
  (fn [rocks]
    (reduce (scoot-col rows) rocks (range cols))))

(defn rotate [[rows _cols]]
  (fn [rocks]
    (update-keys rocks (fn [[r c]] [c (- rows r 1)]))))

(defn score [[rows _cols] rocks]
  (transduce (comp
              (keep (fn [[[r _c] v]] (when (= v \O) r)))
              (map #(- rows %)))
             +
             rocks))

(comment
  (do
    (def rocks (into {} (common2/locmap<- #{\# \O}) (slurp "input/2023/14-sample.txt")))
    (def size [10 10]))
  (do
    (def rocks (into {} (common2/locmap<- #{\# \O}) (slurp "input/2023/14-rocks.txt")))
    (def size [100 100]))

  ;; year 2023 day 14 puzzle 1
  (score size ((scoot size) rocks))
  ;; => 108759

  ;; I explored the patterns (in v1) and know that after 155 cycles, the pattern
  ;; repeats with a period of 17.

  ;; year 2023 day 14 puzzle 2
  (time
   (let [prime 155
         period 17
         align (rem (- 1000000000 prime) period)
         billionth (->> rocks
                        (iterate (comp (rotate size) (scoot size)))
                        (drop (* 4 (+ prime align)))
                        first)]
     (score size billionth)))
  ;; => 89089


  )