(ns advent-of-code.2015.day-8
  (:require [clojure.string :as str]))

;; My approach here was not to actually _do_ the escaping, just count up how 
;; many times escaping was needed, and use that to determine the new length.

(defn escape-count [s]
  (let [escapes (re-seq #"\\\"|\\\\|\\x[0-9a-f-A-F]{2}" s)]
    (- (count s)   ; from raw string length ...
       2           ; ... subtract two for outer quotes ...
                   ; ... and one less than length of each escape sequence
       (apply + (map (comp dec count) escapes)))))  

(defn puzzle1 [in]
  (->> in
       str/split-lines
       (map (juxt count escape-count))
       (map #(apply - %))
       (reduce +)))

(comment
  (let [input (slurp "input/2015/8-example_strings.txt")]
    (puzzle1 input))
  (let [input (slurp "input/2015/8-strings.txt")]
    (puzzle1 input)))

(defn re-escape-count [s]
  (let [single-escapes (re-seq #"\\\"|\\\\" s)
        hex-escapes (re-seq #"\\x[0-9a-f-A-F]{2}" s)]
    (+
     (count s)                      ; raw string length
     4                              ; escaping outer quotes adds 4 chars
     (* 2 (count single-escapes))   ; single escapes add 2 chars
     (count hex-escapes))))         ; hex escapes add 1 char

(defn puzzle2 [in]
  (->> in
       str/split-lines
       (map (juxt re-escape-count count))
       (map #(apply - %))
       (reduce +)))

(comment
  (let [input (slurp "input/2015/8-example_strings.txt")]
    (puzzle2 input))
  (let [input (slurp "input/2015/8-strings.txt")]
    (puzzle2 input)))
