(ns advent-of-code.2015.day-19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 19: Medicine for Rudolph ---

;; I want to redo this part 1, too. We can scan the rules (and the string) with 
;; #"[A-Z][a-z]?|[a-z]" and it will give us every molecule: Ca, C, P, B, Rn.
;; Then we find out which molecules never appear on the right side of a
;; production rule. (For my input it is C, Rn, Y, Ar, maybe some others and 
;; maybe different for other inputs.) Then we split the input string by the 
;; sequence of molecules and partition by those terminal values. Then, stepping
;; through the partitions if a subgroup has only 1 non-terminal, we replace it 
;; by the number of values it can produce, as it can't produce any combinations
;; that would be repeats. Terminals are replaced by '1'. Larger groups of
;; non terminal we can brute-force using a cartesian product and removing any 
;; resulting duplicates, then replace by count.
;; The final number is the product of all the reduced-to-a-number partitions.

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

;; after reading posts on reddit, there are some clever and quick ways to
;; solve part 2. The production rules are of two types:
;;   X => XX where X is any of the molecules _except_ Rn, Ar, or Y
;;   X => X(X) | X(X,X) | X(X,X,X) where Rn, Ar, and Y are '(', ',', ')'
;; So an attack plan is to find these (X)-type patterns and reduce those first.
;; A pattern that ends with Ar can't be affected by anything to the right 
;; (because Ar is not on the left side of any production rule), it works well
;; to reduce from the right end of the string (i.e., do it in reverse)
;; I could turn this into an actual nested list structure and then reduce it
;; with postwalk, I'm just not sure how to keep track of the count of things
;; if I do that.

(defn puzzle2 [input]
  (let [[lines s] (str/split input #"\n\n")
        rules (->> (str/split-lines lines)
                   (map #(edn/read-string (str "(" % ")")))
                   (map (fn [[a _ b]] {(str b) (str a)}))
                   (apply merge-with into))]
    rules))

(comment
  (let [input (slurp "input/2015/19-molecules.txt")] (puzzle2 input)))