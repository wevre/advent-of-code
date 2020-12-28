(ns advent-of-code.2020.day-6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn puzzle [in rf]
  (->> (str/split in #"\n\n")
       (map str/split-lines)
       (map (fn [s] (reduce rf (map set s))))
       (map count)
       (reduce +)))

(comment
  (puzzle (slurp "input/2020/6-customs.txt") set/union)
  (puzzle (slurp "input/2020/6-customs.txt") set/intersection)
  
  (let [input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"]
    [(puzzle input set/union)
     (puzzle input set/intersection)])
  )
