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

(defn filter-bits [input test]
  (loop [acc [] [v & rest :as input] input]
    (if (nil? rest)
      (concat acc v)
      (let [bits (reduce reduce-bits nil input)
            most-common (test (first bits))
            filtered (->> input (filter #(= most-common (first %))) (map next))]
        (recur (conj acc most-common) filtered)))))

(defn puzzle2 [file]
  (let [input (parse (slurp file))
        test-oxygen (fn [b] (if (neg? b) "0" "1"))
        test-co2 (fn [b] (if (neg? b) "1" "0"))
        oxygen (dec<-bin (apply str (filter-bits input test-oxygen)))
        co2 (dec<-bin (apply str (filter-bits input test-co2)))]
    (* oxygen co2)))

(comment
  (puzzle2 "input/2021/3-diagnostics.txt")
  
  (let [data "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
        input (parse data)
        test-oxygen (fn [b] (if (neg? b) "0" "1"))
        test-co2 (fn [b] (if (neg? b) "1" "0"))
        oxygen (dec<-bin (apply str (filter-bits input test-oxygen)))
        co2 (dec<-bin (apply str (filter-bits input test-co2)))]
    (println "oxygen rating is " oxygen)
    (println "co2 rating is " co2))
  

  )