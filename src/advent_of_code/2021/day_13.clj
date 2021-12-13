(ns advent-of-code.2021.day-13
  (:require [clojure.string :as str]
            [advent-of-code.common :refer [parse-longs]]))

(defn parse-input [s]
  (let [[dots folds] (str/split s #"\n\n")
        dot-map (->> (parse-longs dots)
                     (partition 2)
                     (map #(zipmap [:x :y] %))
                     set)
        folds (->> (re-seq #"(x|y)=(\d+)" folds)
                   (map (fn [[_ axis line]]
                          {:axis (keyword axis)
                           :line (parse-long line)})))]
    {:dots dot-map :folds folds})
  )

(defn do-fold [dots {:keys [axis line]}]
  (let [mirror (fn [val] (if (< val line) val (- (* 2 line) val)))]
    (reduce (fn [acc dot] (conj acc (update dot axis mirror))) #{} dots)))

(defn string<-dots [dots]
  (let [{width :x height :y} (update-vals (apply merge-with max dots) inc)]
    (->> (for [y (range height) x (range width)]
           (if (dots {:x x :y y}) \# \.))
         (partition width)
         (map #(apply str %))
         (str/join "\n"))))

(comment
  (let [{:keys [dots folds]} (parse-input (slurp "input/2021/13-dots-test.txt"))]
    (print (string<-dots (reduce do-fold dots folds #_(take 1 folds))))))

(comment
  ;; Puzzle 1
  (let [{:keys [dots folds]} (parse-input (slurp "input/2021/13-dots.txt"))]
    (count (reduce do-fold dots (take 1 folds))))

  ;; Puzzle 2
  (let [{:keys [dots folds]} (parse-input (slurp "input/2021/13-dots.txt"))]
    (print (string<-dots (reduce do-fold dots folds))))
  )