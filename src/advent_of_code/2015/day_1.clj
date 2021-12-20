(ns advent-of-code.2015.day-1)

;; --- Day 1: Not Quite Lisp ---
;; https://adventofcode.com/2015/day/1

(comment
  ;; part 1
  (->> (slurp "input/2015/1-floors.txt")
       (map {\( +1, \) -1})
       (reduce +))   ;=> 280

  ;; part 2
  (->> (slurp "input/2015/1-floors.txt")
       (map {\( +1, \) -1})
       (reductions + 0)
       (take-while (comp not neg?))
       count)   ;=> 1797
  )
