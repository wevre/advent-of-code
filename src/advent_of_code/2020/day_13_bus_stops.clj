(ns advent-of-code.2020.day-13-bus-stops
  (:require [advent-of-code.common.remainders :as remainders]
            [clojure.edn :as edn]))

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

(defn puzzle2 [in]
  (->> (rest (re-seq #"(\d+)|x" in))
       (keep-indexed (fn [i [_ x]]
                       (when-let [x (and x (edn/read-string x))]
                         [x (mod (- x i) x)])))
       remainders/congruences))

(comment
  (puzzle2 (slurp "input/2020/13-bus_ids.txt"))   ; => 702970661767766
  )
