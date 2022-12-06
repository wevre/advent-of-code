(ns advent-of-code.2020.day-06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 6: Custom Customs ---

(defn puzzle [rf input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map (fn [s] (reduce rf (map set s))))
       (map count)
       (reduce +)))

(comment
  (puzzle set/union (slurp "input/2020/6-customs.txt"))
  (puzzle set/intersection (slurp "input/2020/6-customs.txt"))

  (let [input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"]
    [(puzzle set/union input)
     (puzzle set/intersection input)]))
