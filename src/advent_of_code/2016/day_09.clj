(ns advent-of-code.2016.day-09
  (:require [clojure.java.io :as io]))

;;;; --- Day 9: Explosives in Cyberspace ---
;;;; https://adventofcode.com/2016/day/9

(def skip? true)   ; Really: `ignore-embedded-markers?`

(defn get-skip [marker weights ptr]
  (let [[skip times] (map parse-long (re-seq #"\d+" (apply str marker)))]
    (doseq [i (range skip)
            :let [i (+ ptr i)]]
      (aset weights i (* times (aget weights i))))
    skip))

(defn decompress [chars weights]
  (loop [sum 0 ptr 0 mark nil skip 0]
    (let [c (nth chars ptr nil)
          w (nth weights ptr 0)]
      #_(println "sum" sum "ptr" ptr "mark" mark "skip" skip "c" c "w" w)
      (cond
        (<= (count chars) ptr) sum
        (and mark (= \) c))
        (let [marker (subvec chars mark (inc ptr))
              skip (get-skip marker weights (inc ptr))]
                               (recur sum       (inc ptr) nil skip))
        mark                   (recur sum       (inc ptr) mark skip)
        (and skip? (< 0 skip)) (recur (+ sum w) (inc ptr) mark (dec skip))
        (= \( c)               (recur sum       (inc ptr) ptr  skip)
        :else                  (recur (+ sum w) (inc ptr) mark skip)))))

(comment
  ;; part 1
  (let [chars (vec (slurp "input/2016/09-compress.txt"))
        weights (long-array (count chars) 1)]
    (decompress chars weights))   ;=> 74532

  ;; part 2
  (with-redefs [skip? false]
    (let [chars (vec (slurp "input/2016/09-compress.txt"))
          weights (long-array (count chars) 1)]
      (decompress chars weights)))   ;=> 11558231665
  )
