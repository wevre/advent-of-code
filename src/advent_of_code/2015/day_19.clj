(ns advent-of-code.2015.day-19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 19: Medicine for Rudolph ---

(defn parse-rules [lines]
  (->> (str/split-lines lines)
       (map #(edn/read-string (str "(" % ")")))
       (map (fn [[a _ b]] {(str a) [(str b)]}))
       (apply merge-with into)))

(defn expansions [m [s & r]]
  (if (nil? s)
    ()
    (into (map #(cons % r) (m s [s])) (map #(cons s %) (expansions m r)))))

(defn puzzle1 [input]
  (let [[lines s] (str/split input #"\n\n")
        rules (parse-rules lines)
        ss (re-seq (re-pattern (str (str/join "|" (keys rules)) "|.+?")) s)]
    (count (disj (into #{} (map #(apply str %) (expansions rules ss))) s))))

(comment
  (let [input (slurp "input/2015/19-molecules.txt")]
    (puzzle1 input))

  (let [input "H => HO
H => OH
O => HH

HOH"]
    (puzzle1 input)))

; See discussion on Reddit for Day 19 solutions.

(defn puzzle2 [input]
  (let [[_ s] (str/split input #"\n\n")
        ss (re-seq #"[A-Z][a-z]?|[a-z]" s)
        freqs (frequencies ss)]
    (- (count ss) (freqs "Ar") (freqs "Rn") (* 2 (freqs "Y")) 1)))

(comment
  (let [input (slurp "input/2015/19-molecules.txt")] (puzzle2 input)))
