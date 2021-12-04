(ns advent-of-code.2021.day-3
  (:require [clojure.string :as str]))

;; --- Day 3: Binary Diagnostic ---

(defn parse [input]
  (->>
   input
   str/split-lines
   (map #(re-seq #"\d" %))))

(defn reduce-bits [acc val]
  (let [acc (if (nil? acc) (repeat (count val) 0) acc)
        combine (fn [a v]
                  (+ a (if (= "1" v) 1 -1)))]
    (map combine acc val)))

(defn dec<-bin [s] (Integer/parseInt s 2))

(defn puzzle1 [file]
  (let [input (parse (slurp file))
        bits (reduce reduce-bits nil input)
        gamma (dec<-bin (apply str (map #(if (pos? %) "1" "0") bits)))
        epsilon (dec<-bin (apply str (map #(if (neg? %) "1" "0") bits)))]
    (* gamma epsilon)))

(comment
  (puzzle1 "input/2021/3-diagnostics.txt")

  (parse (slurp "input/2021/3-diagnostics.txt"))

  (reduce-bits nil '("1" "0" "1" "0"))

  (reduce-bits '(1 -1 1 -1) '("1" "1" "1" "1"))

  (reduce-bits '(1 -1 1 -1) '("0" "0" "0" "0"))

  (reduce reduce-bits nil (parse (slurp "input/2021/3-diagnostics.txt")))

  (map #(Integer/parseInt (apply str %) 2) (parse (slurp "input/2021/3-diagnostics.txt")))

  (map #(dec<-bin (apply str %)) (parse (slurp "input/2021/3-diagnostics.txt")))
  )