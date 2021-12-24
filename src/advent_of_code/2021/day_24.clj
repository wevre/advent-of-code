(ns advent-of-code.2021.day-24
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]))

;; read in the input and create a machine to execute it.

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(str "(" % ")"))
       (map edn/read-string)
       (postwalk #(if (symbol? %) (keyword %) %))))

(comment
  (let [input (parse-input (slurp "input/2021/24-alu.txt"))]
    (pprint
     (->> input
          (partition 18)
          (map vec)
          (map #(select-keys % [4 5 15]))))
    #_
    (pprint
     (->> (frequencies input)
          (remove #(#{14 28} (val %)))))))


(defn alu [code]
  (fn [input]
    (let [input (->> (Long/toString input 9) seq (map #(inc (Character/digit % 10))))]
      (loop [{input :input :as state} {:w 0 :x 0 :y 0 :z 0 :input input} [[op a b] & code] code]
        (let [b (state b b)]
          (case op
            :inp (recur (assoc state a (first input) :input (rest input)) code)
            :add (recur (update state a + b) code)
            :mul (recur (update state a * b) code)
            :div (recur (update state a quot b) code)
            :mod (recur (update state a mod b) code)
            :eql (recur (update state a #(if (= %1 %2) 1 0) b) code)
            state))))))

(defn monad [input]
  (->> input
   (map #(Character/digit % 10))
   (map inc)))

(defn solve [monad nums]
  (loop [[num & nums] nums]
    (if (zero? (:z (monad num)))
      num
      (recur nums))))

(comment

  (time
   (let [input (parse-input (slurp "input/2021/24-alu.txt"))
         monad (alu input)]
     (->> (iterate dec 9r88888888888888)
          (map monad)
          (take 1)
          first #_#_
          (drop-while #(not= 0 (:z %)))
          first)))

  (require '[clojure.pprint :refer [pprint]])

  (pprint
   (->> (iterate dec 9r88888888888888)
        (map #(Long/toString % 9))
        (take 20)))

  (parse-input (slurp "input/2021/24-alu.txt"))

  (let [input (parse-input "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2
")
        alu (alu input)]
    (alu '(7))))