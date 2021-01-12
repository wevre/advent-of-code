(ns advent-of-code.2015.day-10)

;; --- Day 10: Elves Look, Elves Say ---

(defn look-and-say [x]
  (->> x
       (partition-by identity)
       (mapcat (juxt count first))
       (apply str)))

(defn puzzle [cnt input]
  (count (nth (iterate look-and-say input) cnt)))

(comment
  (let [input "1113222113"]
    [(puzzle 40 input) (puzzle 50 input)]))
