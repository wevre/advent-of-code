(ns advent-of-code.2017.day-01-digits)

(defn matching-digits [dd's cnt offset]
  (let [s1 (cycle dd's)
        s2 (drop offset s1)]
    (->> (map vector s1 s2)
         (take cnt)
         (filter #(apply = %))
         (map first))))

(comment

  (do
    (def input (slurp "input/2017/01-digits.txt") #_"12131415")
    (def cnt (count input))
    (def digits (map parse-long (re-seq #"\d" input))))

  ;; year 2017 day 01 puzzle 1
  (reduce + (matching-digits digits cnt 1))

  ;; year 2017 day 01 puzzle 2
  (reduce + (matching-digits digits cnt (quot cnt 2))))
