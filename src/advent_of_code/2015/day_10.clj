(ns advent-of-code.2015.day-10)

;;;; --- Day 10: Elves Look, Elves Say ---
;;;; https://adventofcode.com/2015/day/10

(defn look-and-say [x]
  (->> x
       (partition-by identity)
       (mapcat (juxt count first))
       (apply str)))

(defn solve [n input]
  (->> (iterate look-and-say input)
       (drop n)
       first
       count))

(def input "1113222113")

(comment
  ;; part 1 -- 458ms
  (time
   (solve 40 input))   ;=> 252594

  ;; part 2 -- 6.4s
  (time
   (solve 50 input))   ;=> 3579328
  )
