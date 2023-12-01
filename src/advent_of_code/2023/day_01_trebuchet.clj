(ns advent-of-code.2023.day-01-trebuchet
  (:require [clojure.string :as str]))

(def digit<-word {"one"   "1", "two"   "2", "three" "3",
                  "four"  "4", "five"  "5", "six"   "6",
                  "seven" "7", "eight" "8", "nine"  "9"})

(defn outer-digits [s re]
  (->> s
       (re-seq re)   ;; NOTE: with capture groups, re-seq returns a vector
       (map second)
       ((juxt first last))
       (map #(digit<-word % %))))

(defn number<- [s] (parse-long (apply str s)))

(defn calibration-number<- [lines re]
  (transduce
   (comp (map #(outer-digits % re)) (map number<-))
   + lines))

(comment

  (let [input (slurp "input/2023/01-calibration.txt")
        #_#_input "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"]
    (def lines (str/split-lines input)))

  ;; year 2023 day 01 puzzle 1
  (calibration-number<- lines #"(\d)")

  ;; year 2023 day 01 puzzle 2
  (calibration-number<- lines #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))"))
