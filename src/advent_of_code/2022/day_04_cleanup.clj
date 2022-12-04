(ns advent-of-code.2022.day-04-cleanup
  (:require [clojure.string :as str]))

(defn parse-pairs [s]
  (->> (str/split s #"-|,")
       (map parse-long)))

(defn puzzle [pred input]
  (->> input
       str/split-lines
       (map parse-pairs)
       (filter pred)
       count))

(defn fully-contains? [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn overlap-at-all? [[a b c d]]
  (and (<= c b) (<= a d)))

(comment
  ;; puzzle 1
  (puzzle fully-contains? (slurp "input/2022/04-cleanup.txt"))   ; => 657

  ;; puzle 2
  (puzzle overlap-at-all? (slurp "input/2022/04-cleanup.txt"))   ; => 938
  )
