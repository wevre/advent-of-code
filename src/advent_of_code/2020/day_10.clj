(ns advent-of-code.2020.day-10
  (:require [clojure.string :as str]))

(defn diffs [in]
  (->> (str/split-lines in)
       (map #(Integer/parseInt %))
       (cons 0)
       sort
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn puzzle1 [in]
  (let [freqs (frequencies (diffs in))]
    (* (freqs 1) (inc (freqs 3)))))

;; This function works for arbitrarily large sequences of 1-diffs. But the 
;; largest I actually encountered in the input was four in a row. Knowing that,
;; we could just create a map from 'length of 1-diff run' to 'count of combos'
;; and use that as a lookup. Here is what that map looks like:
;; {1 1, 2 1, 3 2, 4 4, 5 7, 6 13, 7 44, ..., i (count-combos (range (inc i)))}
;; 
;; One solution shared on github used a formula with binomial coefficients, but
;; it was incorrect for larger diff runs. Their solution was correct only 
;; because the input never had larger runs than length 4.

(def count-combos
  (let [maybe-count
        (fn [v xs] (and (seq xs) (<= (- (first xs) v) 3) (count-combos xs)))]
    (memoize
     (fn [[v & vs]]
       (if (not v)
         1
         (+ (count-combos vs)
            (or (maybe-count v (rest vs)) 0)
            (or (maybe-count v (nthrest vs 2)) 0)))))))

(defn puzzle2 [in]
  (->> (diffs in)
       (partition-by #(= 3 %))
       (map #(if (= 3 (first %)) 1 (count-combos (range (inc (count %))))))
       (reduce *)))

(comment
  (puzzle1 (slurp "input/2020/10-jolts.txt"))
  (puzzle2 (slurp "input/2020/10-jolts.txt"))
  (puzzle2 (slurp "input/2020/10-jolts_alt.txt"))
  
  (let [input "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3\n"]
    [(puzzle1 input)
     (puzzle2 input)])

  (let [input "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4\n"]
    [(puzzle1 input)
     (puzzle2 input)]))