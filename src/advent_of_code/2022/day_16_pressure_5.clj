(ns advent-of-code.2022.day-16-pressure-5
  (:require [clojure.string :as str]))

;; 2022-12-21
;;    Checking out solution by @misha
;;    https://github.com/akovantsev/adventofcode/blob/master/src/adventofcode/y2022/day16.clj
;;    The code is much more elegant than mine, but it is also much slower.

(def START "AA")
(def TIMELIMIT1 30)
(def TIMELIMIT2 26)

(defn parse [input]
  (->> input
       str/split-lines
       (map #(re-seq #"[A-Z]{2}|\d+" %))
       (map (fn [[f p & r]] [f {::rate (parse-long p) ::ids (set r)}]))
       (into {})))


(defn p1 [ss]
  (let [DB    (parse ss)
        OPEN  (->> DB (filter #(-> % val ::rate zero?)) (map key) set)
        START {[START OPEN] 0}

        step  (fn step [[[at opened] score]]
                (let [ids     (-> at DB ::ids)
                      opened+ (conj opened at)
                      score+  (->> opened (map DB) (map ::rate) (reduce + score))
                      state+  (fn m1 [id] {[id opened] score+})
                      moves   (map state+ ids)
                      open    {[at opened+] score+}]
                  (conj moves open)))

        tick  (fn tick [best]
                (->> best (mapcat step) (reduce (partial merge-with max))))]

    (->> START (iterate tick) (drop TIMELIMIT1) first vals (reduce max))))

(comment
  ;; about 24 seconds
  (time
   (p1 (slurp "input/2022/16-pressure.txt")))
  )
