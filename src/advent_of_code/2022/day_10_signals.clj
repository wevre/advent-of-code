(ns advent-of-code.2022.day-10-signals
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(edn/read-string (str "[" % "]")))
       #_(mapcat #(if (= 'addx (first %)) [0 (second %)] [0]))
       (mapcat #(assoc % 0 0))   ; borrowed trick from @tschady
       (reductions + 1)))

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/10-signals.txt"))
       (map vector (drop 1 (range)))
       (filter #(#{20 60 100 140 180 220} (first %)))
       (map #(apply * %))
       (apply +))   ; => 17020

  ;; puzzle 2
  (->> (parse (slurp "input/2022/10-signals.txt"))
       (map vector (cycle (range 40)))
       (map #(if (<= -1 (apply - %) 1) \# \.))
       (partition 40)
       (map str/join))   ; => RLEZFLGE
  )