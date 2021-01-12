(ns advent-of-code.2015.day-8
  (:require [clojure.string :as str]))

;; --- Day 8: Matchsticks ---

(defn escape-count [s]
  (let [escapes (re-seq #"\\\"|\\\\|\\x[0-9a-f-A-F]{2}" s)]
    (- (count s)   ; from raw string length ...
       2           ; ... subtract two for outer quotes ...
                   ; ... remove all but one character of each escape sequences
       (->> escapes (map (comp dec count)) (reduce +)))))  

(defn re-escape-count [s]
  (let [single-escapes (re-seq #"\\\"|\\\\" s)
        hex-escapes (re-seq #"\\x[0-9a-f-A-F]{2}" s)]
    (+
     (count s)                      ; raw string length
     4                              ; escaping outer quotes adds 4 chars
     (* 2 (count single-escapes))   ; single escapes add 2 chars
     (count hex-escapes))))         ; hex escapes add 1 char

(defn puzzle [counter input]
  (->> (str/split-lines input)
       (map counter)
       (reduce +)))

(comment
  (let [input (slurp "input/2015/8-strings.txt")]
    (puzzle #(- (count %) (escape-count %)) input))

  (let [input (slurp "input/2015/8-strings.txt")]
    (puzzle #(- (re-escape-count %) (count %)) input))

  (let [input (slurp "input/2015/8-example_strings.txt")]
    (puzzle #(- (count %) (escape-count %)) input))

  (let [input (slurp "input/2015/8-example_strings.txt")]
    (puzzle #(- (re-escape-count %) (count %)) input)))
