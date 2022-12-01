(ns advent-of-code.2015.day-19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;;; --- Day 19: Medicine for Rudolph ---
;;;; https://adventofcode.com/2015/day/19

(defn parse-rules [s]
  (->> (str/split-lines s)
       (map #(edn/read-string (str "(" % ")")))
       (map (fn [[a _ b]] {(str a) [(str b)]}))
       (apply merge-with into)))

(defn parse-input [s]
  (let [[rules molecule] (str/split s #"\n\n")
        rules (parse-rules rules)
        molecule (re-seq #"[A-Z][a-z]?|[a-z]" molecule)](map)
    {:rules rules :molecule molecule}))

(defn expansions [rules]
 (fn expansions [[s & r]]
   (if (nil? s)
     ()
     (into (map #(cons % r) (rules s [s])) (map #(cons s %) (expansions r))))))

(comment
  ;; part 1
  (let [{:keys [rules molecule]} (parse-input (slurp "input/2015/19-molecules.txt"))]
    (->> (map #(apply str %) ((expansions rules) molecule))
         set
         count
         dec))   ;=> 518
  )

;; See discussion on Reddit for Day 19 solutions.
;; https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/

(comment
  ;; part 2
  (let [input (slurp "input/2015/19-molecules.txt")
        [_ s] (str/split input #"\n\n")
        mol (re-seq #"[A-Z][a-z]?|[a-z]" s)
        freqs (frequencies mol)]
    (- (count mol) (freqs "Ar") (freqs "Rn") (* 2 (freqs "Y")) 1))   ;=> 200
  )
