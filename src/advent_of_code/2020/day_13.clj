(ns day-13
  (:require [clojure.string :as str]))

(defn puzzle1 [in]
  (let [[ts & ids] (map #(Long/parseLong %) (re-seq #"\d+" in))]
    (->> (map (juxt #(- % (rem ts %)) identity) ids)
         sort
         first
         (reduce *))))

(comment
  (puzzle1 (slurp "input/2020/13-bus_ids.txt")))

(defn gcd [a b] (if (= 0 b) a (recur b (mod a b))))

(defn lcm [a b] (/ (* a b) (gcd a b)))

;; This is the "simpler" method, as opposed to the Gauss's algorithm, which
;; is faster. https://www.di-mgt.com.au/crt.html
(defn puzzle2 [in]
  (->> (rest (re-seq #"(\d+)|x" in))
       (map second)
       (map #(when % (Long/parseLong %)))
       (map-indexed (fn [i x] (when x [x (mod (- x i) x)])))
       (filter some?)
       (reduce (fn [[n1 t] [n2 a2]]
                 (loop [t t]
                   (if (= (mod t n2) a2)
                     [(lcm n1 n2) t]
                     (recur (+ t n1))))))
       second))

(comment 
  (puzzle2 (slurp "input/2020/13-bus_ids.txt")))
