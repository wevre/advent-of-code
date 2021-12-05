(ns advent-of-code.2021.day-3
  (:require [clojure.string :as str]))

;; --- Day 3: Binary Diagnostic ---

(def file "input/2021/3-diagnostics.txt")

(defn parse [input] (->> input str/split-lines (map #(re-seq #"\d" %))))

(defn dec<-bin [s] (Integer/parseInt s 2))

(defn most-common [freq-map]
  (let [ones (get freq-map "1") zeros (get freq-map "0")]
    (if (<= zeros ones) "1" "0")))

(defn least-common [freq-map]
  (let [ones (get freq-map "1") zeros (get freq-map "0")]
    (if (<= zeros ones) "0" "1")))

(defn puzzle1 [file]
  (let [freqs (->> (parse (slurp file)) (apply map vector) (map frequencies))
        gamma (dec<-bin (apply str (map most-common freqs)))
        epsilon (dec<-bin (apply str (map least-common freqs)))]
    (* gamma epsilon)))

(comment
  (puzzle1 file)

  (parse (slurp file))

  (->>
   (parse (slurp file))
   (apply map vector)
   (map frequencies)
   count)
  
  (map #(Integer/parseInt (apply str %) 2) (parse (slurp file)))

  (map #(dec<-bin (apply str %)) (parse (slurp file))))

(defn find-rating 
  "Test the first element of each list in the input, then recur on each lists's
   next element, filtering along the way."
  [input testfn]
  (loop [acc [] [i & is :as input] input]
    (if (nil? is)
      (concat acc i)
      (let [mask (testfn (frequencies (map first input)))]
        (recur (conj acc mask) (map next (filter #(= mask (first %)) input)))))))
        
(defn puzzle2-alt [file]
  (let [input (parse (slurp file))
        oxygen (dec<-bin (apply str (find-rating input most-common)))
        co2 (dec<-bin (apply str (find-rating input least-common)))]
    (* oxygen co2)))

(comment
  (puzzle2-alt file))
