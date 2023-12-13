(ns advent-of-code.2023.day-12
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn subs* [row i]
  (if (< i (count row))
    (subs row i)
    ""))

(def ?op #{\. \?})
(def ?dam #{\# \?})

(defn ?valid [row b]
  (and (<= b (count row))
       (every? ?dam (subs row 0 b))
       (or (= b (count row)) (?op (nth row b)))))

(def solve
  (memoize
   (fn [row b's]
     (if-not (first b's)
       (if (every? ?op row) 1 0)
       (let [space (- (count row) (reduce + b's) (max 0 (dec (count b's))))]
         (if (< space 0)
           0
           (loop [[b & rb's :as b's] b's i 0 c 0]
             (if-not (<= i space)
               c
               (let [c (+ c (if (?valid (subs row i) b)
                              (solve (subs* row (+ i b 1)) rb's)
                              0))]
                 (if (?op (nth row i))
                   (recur b's (inc i) c)
                   c))))))))))

(defn parse-line [line]
  (let [[row b's] (str/split line #" ")]
    [row (common/parse-longs b's)]))

(defn unfold [[row b's]]
  [(str/join \? (repeat 5 row)) (apply concat (repeat 5 b's))])

(comment
  (def lines (str/split-lines (slurp "input/2023/12-springs.txt")))
  (def lines (str/split-lines "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1\n"))

  ;; year 2023 day 12 puzzle 1
  (time
   (transduce (comp (map parse-line)
                    (map #(apply solve %)))
              +
              lines))
  ;; => 6488


  ;; year 2023 day 12 puzzle 1
  (time
   (transduce (comp (map parse-line)
                    (map unfold)
                    (map #(apply solve %)))
              +
              lines))
  ;; => 815364548481 (time ~11 msecs)

  )