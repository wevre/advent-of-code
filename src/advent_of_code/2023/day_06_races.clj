(ns advent-of-code.2023.day-06-races
  (:require [clojure.math :as math]))

(defn beat [time record]
  (fn [i] (< record (* i (- time i)))))

(defn wins [[time record]]
  (->> (range time)
       (filter (beat time record))
       count))

(comment
  (let [doc [[7 9] [15 40] [30 200]]]
    (->> doc
         (map wins)
         (reduce *)))

  ;; year 2023 day 06 puzzle 1
  (let [doc [[60 601] [80 1163] [86 1559] [76 1300]]]
    (->> doc
         (map wins)
         (reduce *)))   ;; => 1155175

  ;; year 2023 day 06 puzzle 2 -- takes about 4 seconds
  (let [doc [[60808676 601116315591300]]]
    (->> doc
         (map wins)
         (reduce *)))   ;; => 35961505

  ;; year 2023 day 06 puzzle 2 -- visual inspection
  (let [[time record] [60808676.0 601116315591300.0]
        n1 (long (/ (- time (math/sqrt (- (* time time) (* 4 record)))) 2.0))
        n2 (long (/ (+ time (math/sqrt (- (* time time) (* 4 record)))) 2.0))]
    (for [nudge (range -1 2)
          :let [n1 (+ n1 nudge) n2 (+ n2 nudge)]]
      (map (juxt identity (beat time record)) [n1 n2])))
  ;; => (([12423584 false] [48385089 true])
  ;;     ([12423585 false] [48385090 true])
  ;;                        ^^^^^^^^
  ;;     ([12423586 true] [48385091 false]))
  ;;       ^^^^^^^^

  (inc (- 48385090 12423586))   ;; => 35961505

  )