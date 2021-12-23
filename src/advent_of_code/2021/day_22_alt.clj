(ns advent-of-code.2021.day-22-alt
  (:require [clojure.string :as str]))

;;;; --- Day 22: Reactor Reboot ---
;;;; https://adventofcode.com/2021/day/22

;;; I did this to see how to implement pos/neg volumes, like many did. But it
;;; turns out to be _much_ slower than my original solution which split cubes.
;;; Why? Maybe because this approach only adds cubes, to the tune of n*(n+1)/2
;;; so it grows kind of large? The other one is more like pruning: absorbed
;;; cubes are dropped along the way.

;;; Don't know if this proves it, but this negacube approach generates 34,661
;;; cubes to be added up in the end, whereas the split approach only 3,948.


;;; Parse input.

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(re-seq #"on|off|-?\d+" %))
       (map (fn [[cmd & coords]]
              {:op ({"on" +1 "off" -1} cmd)
               :cube (->> (map parse-long coords) (partition 2))}))))

;;; Wrangling cubes.

(defn negacube [{a-op :op a-cube :cube} {b-cube :cube}]
  (let [res (map (fn [[a-min a-max] [b-min b-max]]
                   (list (max a-min b-min) (min a-max b-max)))
                 a-cube b-cube)]
    (when (every? #(apply <= %) res) {:op (- 0 a-op) :cube res})))

(defn volume [{:keys [op cube]}]
  (reduce * op (map (fn [[a b]] (inc (- b a))) cube)))

(defn reboot [steps]
  (reduce (fn [cubes {op :op :as cube}]
            (cond-> (into cubes (keep #(negacube % cube) cubes))
              (= +1 op) (conj cube))
            )
          []
          steps))

(comment
  ;; part 2 -- 11680ms
  (let [input (parse-input (slurp "input/2021/22-cubes.txt"))]
    (time
     (->> input
          #_(take 20)
          reboot
          count
          #_#_
          (map volume)
          (reduce +)))))

(comment
  (let [a [[-10 10] [-10 10] [-10 10]]
        b [[13 14] [-13 13] [-13 13]]]
    (negacube a b))

  (volume [[-3 3] [-3 3] [-3 3]]))