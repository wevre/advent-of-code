(ns day-13
  (:require [clojure.edn :as edn]))

;; --- Day 13: Shuttle Search ---

; Uses a simple implemenation of Chinese remainder theorem. A more complicated
; (and I suppose, efficient) algorithm, Gauss's, is described here
; https://www.di-mgt.com.au/crt.html. 

(defn puzzle1 [in]
  (let [[ts & ids] (map edn/read-string (re-seq #"\d+" in))]
    (->> (map (juxt #(- % (rem ts %)) identity) ids)
         sort
         first
         (reduce *))))

(comment
  (puzzle1 (slurp "input/2020/13-bus_ids.txt")))

(defn gcd [a b] (if (= 0 b) a (recur b (mod a b))))

(defn lcm [a b] (/ (* a b) (gcd a b)))

(defn puzzle2 [in]
  (->> (rest (re-seq #"(\d+)|x" in))
       (keep-indexed (fn [i [_ x]]
                       (when-let [x (and x (edn/read-string x))]
                         [x (mod (- x i) x)])))
       (reduce (fn [[n1 t] [n2 a2]]
                 (loop [t t]
                   (if (= (mod t n2) a2)
                     [(lcm n1 n2) t]
                     (recur (+ t n1))))))
       second))

(comment 
  (puzzle2 (slurp "input/2020/13-bus_ids.txt")))
