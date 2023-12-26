(ns advent-of-code.2023.day-25-wires-v2
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [lines (map #(re-seq #"[a-z]{3}" %) (str/split-lines input))]
    (reduce (fn [acc [k & r's]]
              (into acc (map #(vector k %) r's)))
            []
            lines)))

(defn set-ify [c] (if (string? c) #{c} c))

(defn contract [edges]
  (let [cnt (count edges)
        n (rand-int cnt)
        [a b] (nth edges n)
        newv (into (set-ify a) (set-ify b))]
    (reduce (fn [acc [c d]]
              (cond
                (or (and (= a c) (= b d)) (and (= a d) (= b c))) acc
                (or (= a c) (= b c)) (conj acc [newv d])
                (or (= a d) (= b d)) (conj acc [c newv])
                :else (conj acc [c d])))
            []
            edges)))

(defn run-karger [input]
  (->> (iterate contract input)
       (drop-while #(< 3 (count %)))
       first))

(comment
  (def input (parse "aaa bbb ccc ddd\nbbb ccc ddd\nccc ddd"))
  (def input (parse (slurp "input/2023/25-sample.txt")))
  (def input (parse (slurp "input/2023/25-wires.txt")))

  ;; year 2023 day 25 puzzle 1
  ;; This doesn't always work. If it doesn't find any solutions, returns 1. But
  ;; every so often, it takes about 40s and returns the answer.
  (time
   (->> (pmap (fn [_] (run-karger input)) (range 100))
        (keep #(when (= 3 (count %)) (map count (first %))))
        (take 1)
        first
        (reduce *)))
  ;; => 571753
  )
