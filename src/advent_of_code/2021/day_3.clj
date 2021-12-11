(ns advent-of-code.2021.day-3
  (:require [clojure.string :as str]))

;; --- Day 3: Binary Diagnostic ---

(def file "input/2021/3-diagnostics.txt")

(defn parse [input] (->> input str/split-lines (map #(re-seq #"\d" %))))

(defn dec<-bin "input is a coll of string 1's or 0's."
  [ss] (Integer/parseInt (apply str ss) 2))

(defn most-common [freq-map]
  (let [ones (get freq-map "1") zeros (get freq-map "0")]
    (if (<= zeros ones) "1" "0")))

(defn least-common [freq-map]
  (let [ones (get freq-map "1") zeros (get freq-map "0")]
    (if (<= zeros ones) "0" "1")))

(defn puzzle1 [file]
  (let [freqs (->> (parse (slurp file)) (apply map vector) (map frequencies))
        gamma (dec<-bin (map most-common freqs))
        epsilon (dec<-bin (map least-common freqs))]
    (* gamma epsilon)))

(comment
  (puzzle1 file)
)

(defn find-rating
  "Find the least/most-common first bit of each list in input, filter out lists
   that don't have that first bit, then recur on the `next` of each list. Stop
   when there is only one list left in the input."
  [input testfn]
  (loop [bits [] [i & is :as input] input]
    (if (nil? is)
      (concat bits i)   ;; Put remaining list `i` back together with previously tested bits.
      (let [mask (testfn (frequencies (map first input)))]
        (recur (conj bits mask)   ;; Keep track of each least/most-common bit.
               (map next (filter #(= mask (first %)) input)))))))

(defn puzzle2 [file]
  (let [input (parse (slurp file))
        oxygen (dec<-bin (find-rating input most-common))
        co2 (dec<-bin (find-rating input least-common))]
    (* oxygen co2)))

(comment
  (puzzle2 file))
