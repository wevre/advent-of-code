(ns advent-of-code.2015.day-10)

(defn look-and-say [x]
  (->> x
       (partition-by identity)
       (mapcat (juxt count first))
       (apply str)))

(defn puzzle1 [cnt in]
  (count (nth (iterate look-and-say in) cnt)))

(comment
  (let [input "1113222113"]
    (puzzle1 50 input)))
