(ns advent-of-code.2023.day-01-trebuchet-v2
  (:require [clojure.string :as str]))

(def digit<-word {"one"   "1", "two"   "2", "three" "3",
                  "four"  "4", "five"  "5", "six"   "6",
                  "seven" "7", "eight" "8", "nine"  "9"})

(defn get-or [m k] (get m k k))

(defn first-digit [txf s src-re]
  (get-or digit<-word (txf (first (re-seq (re-pattern (txf src-re)) (txf s))))))

(defn calib-number<-
  "Create two-digit number from first and last match on string, last match comes
   from reversing both the string and the regex."
  [src-re]
  (fn [s]
    (parse-long (str (first-digit identity s src-re)
                     (first-digit str/reverse s src-re)))))

(comment
  (let [input (slurp "input/2023/01-calibration.txt")
        #_#_input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
        #_#_input "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"]
    (def lines (str/split-lines input)))

  ;; year 2023 day 01 puzzle 1
  (let [src-re (str/join "|" (vals digit<-word))]
    (transduce (map (calib-number<- src-re)) + lines))

  ;; year 2023 day 01 puzzle 2
  (let [src-re (str/join "|" (concat (keys digit<-word) (vals digit<-word)))]
    (transduce (map (calib-number<- src-re)) + lines)))
