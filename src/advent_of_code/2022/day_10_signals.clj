(ns advent-of-code.2022.day-10-signals
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn parse [numbering input]
  ;; Trick from @tschady to directly replace instruction with 0. My original
  ;; version tested for `'addx` and fed [0 x] or [0] into `mapcat`.
  (->> (str/replace input #"addx|noop" "0")
       common/parse-longs
       (reductions + 1)
       (map vector numbering)))

(comment
  ;; puzzle 1
  (->> (parse (drop 1 (range)) (slurp "input/2022/10-signals.txt"))
       (filter #(#{20 60 100 140 180 220} (first %)))
       (map #(apply * %))
       (apply +))   ; => 17020

  ;; puzzle 2
  (->> (parse (cycle (range 40)) (slurp "input/2022/10-signals.txt"))
       (map #(apply - %))
       (map #({-1 \# 0 \# +1 \#} % \.))
       (partition 40)
       (map str/join))   ; => RLEZFLGE
  )
