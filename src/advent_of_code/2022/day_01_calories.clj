(ns advent-of-code.2022.day-01-calories
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn parse [n input]
  (->> (str/split input #"\n\n")
       (map common/parse-longs)
       (map #(reduce + %))
       (sort >)
       (take n)
       (reduce +)))

(comment
  ;; puzzle 1
  (parse 1 (slurp "input/2022/01-calories.txt"))   ; => 66487

  ;; puzzle 2
  (parse 3 (slurp "input/2022/01-calories.txt"))   ; => 197301
  )
