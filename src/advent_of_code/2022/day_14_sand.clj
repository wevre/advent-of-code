(ns advent-of-code.2022.day-14-sand
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]))

;; 2022-12-13 10:56
;;    So sloppy and crude, but I don't care. I got the stars and now I can go
;;    and get some rest for snowboarding tomorrow! Since I already had a
;;    function `points-along` from prior years, I contorted my input into
;;    something I could feed into that function in order to find rocks.
;; 2012-12-14 06:56
;;    Did some cleanup, mostly to function signatures, but didn't change the
;;    logic. I now basically have only the one function, `cascade`. I pulled
;;    `blocked?` inside since it depends on whether or not there is a `floor?`.
;;    I decided to go with nesting `if`'s instead of nesting `cond`'s because
;;    I'm sort of allergic to nested `cond`'s and easier to wrap the branch of
;;    an `if` in a `let` to get some needed values.

(defn- get-points [lines]
  (reduce (fn [acc [a b]] (into acc (common/points-along a b))) #{} lines))

(defn- parse-line [line]
  (->> line common/parse-longs (partition 2) (partition 2 1) get-points))

(defn get-rocks [input]
  (->> input str/split-lines (map parse-line) (reduce into #{})))

(defn cascade [floor? rocks]
  (let [start [500 0]
        lowest (cond-> (reduce max (map second rocks)) floor? (+ 2))
        blocked? (fn [[_ y :as pos] sand]
                   (or (and floor? (= y lowest)) (rocks pos) (sand pos)))]
    (loop [[x y :as pos] start sand #{}]
      (if (or (sand start) (> y lowest))
        sand                                                   ; Done
        (let [down [x (inc y)]
              left [(dec x) (inc y)]
              right [(inc x) (inc y)]]
          (if (blocked? down sand)
            (cond
              (not (blocked? left sand)) (recur left sand)     ; Left
              (not (blocked? right sand)) (recur right sand)   ; Right
              :else (recur start (conj sand pos)))             ; Rest
            (recur down sand)))))))                            ; Down

(comment
  ;; puzzle 1
  (->> (slurp "input/2022/14-sand.txt") get-rocks (cascade false) count)   ; => 683

  ;; puzzle 2
  (time
   (->> (slurp "input/2022/14-sand.txt") get-rocks (cascade true) count))   ; => 28821
  )
