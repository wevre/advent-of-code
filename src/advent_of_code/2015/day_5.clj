(ns advent-of-code.2015.day-5
  (:require [clojure.string :as str]))

(defn nice? [s]
  (and
   (<= 3 (count (take 3 (re-seq #"[aeiou]" s))))
   (some #(apply = %) (partition 2 1 s))
   (empty? (re-seq #"ab|cd|pq|xy" s))))

(defn non-adjacent-pairs? [s]
  (->> (partition 2 1 s)
       (reduce #(conj %1 (and (not= %2 (first %1)) %2)) ())
       frequencies
       (some (fn [[k v]] (and k (< 1 v))))))

(defn nice-v2? [s]
  (and
   (non-adjacent-pairs? s)
   (some (fn [[f _ l]] (= f l)) (partition 3 1 s))))

(defn puzzle1 [in pred]
  (->> in
       str/split-lines
       (filter pred)
       count))

(comment
  (let [input "ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb"] (puzzle1 input nice?))
  
  (let [input (slurp "input/2015/5-naughty_or_nice.txt")] (puzzle1 input nice?))

  (let [input "qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy
xyxy
aabcdefgaa
abdcefeghi"] 
    
    (puzzle1 input nice-v2?))

  (let [input (slurp "input/2015/5-naughty_or_nice.txt")] (puzzle1 input nice-v2?)))
