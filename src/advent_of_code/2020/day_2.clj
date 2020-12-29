(ns advent-of-code.2020.day-2
  (:require [clojure.edn :as edn]))

;; --- Day 2: Password Philosophy ---

(defn valid-password-1? [[l h c s]]
  (let [l (edn/read-string l) h (edn/read-string h)]
    (<= l (count (filter (set c) s)) h)))

(defn valid-password-2? [[l h [c] s]]
  (let [l (dec (edn/read-string l)) h (dec (edn/read-string h))]
    (not= (= c (nth s l)) (= c (nth s h)))))

(defn puzzle [pred input]
  (->> (re-seq #"[\d\w]+" input)
       (partition 4)
       (filter pred)
       count))

(comment
  (puzzle valid-password-1? (slurp "input/2020/2-passwords.txt"))

  (puzzle valid-password-2? (slurp "input/2020/2-passwords.txt"))

  (let [input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"]
    [(puzzle valid-password-1? input)
     (puzzle valid-password-2? input)]))
