(ns advent-of-code.2021.day-22
  (:require [clojure.string :as str]))

;;;; --- Day 22: Reactor Reboot ---
;;;; https://adventofcode.com/2021/day/22


;;; Parse input.

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(re-seq #"on|off|-?\d+" %))
       (map (fn [[cmd & coords]]
              {:command (keyword cmd)
               :cube (->> (map parse-long coords) (partition 2) (mapv #(vec %)))}))))

;;; Wrangling cubes.

(defn outside?-axis [axis a b]
  (let [[a-min a-max] (nth a axis) [b-min b-max] (nth b axis)]
    (or (< b-max a-min) (< a-max b-min))))

(defn outside? [a b] (some #(outside?-axis % a b) [0 1 2]))

(defn split-axis [axis a b]
  (let [[a-min a-max] (nth a axis) [b-min b-max] (nth b axis)]
    (->> [[a-min (dec b-min)] [(inc b-max) a-max]]
         (filter #(apply <= %))
         (into [[(max a-min b-min) (min a-max b-max)]])
         (map #(assoc a axis %)))))

(defn split
  "Return splits of `a` where it overlaps with `b`."
  [a b]
  (if (outside? a b)
    (list a)
    (rest
     (reduce (fn [[a & cs] axis] (concat (split-axis axis a b) cs)) [a] [0 1 2]))))

(defn cube-size [cube]
  (reduce * (map (fn [[a b]] (inc (- b a))) cube)))

;;; Solving.

(defn reboot [steps]
  (reduce (fn [cubes {:keys [command cube]}]
            (cond-> (into #{} (mapcat #(split % cube) cubes))
              (= :on command) (conj cube)))
          #{}
          steps))

(defn solve
  ([steps] (solve steps nil))
  ([steps window]
   (->> (if window (remove #(outside? (:cube %) window) steps) steps)
        (reboot)
        (map cube-size)
        (reduce +))))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2021/22-cubes-test.txt"))]
    (time (solve (take 15 input) [[-50 50] [-50 50] [-50 50]])))

  ;; part 2 -- 575ms
  (let [input (parse-input (slurp "input/2021/22-cubes.txt"))]
    (time (solve input)))
  )

(comment
  (cube-size [[-10 10] [-10 10] [-10 10]]))

(comment
  (let [a [[-10 10] [-10 10] [-10 10]]
        b [[-3 3] [-3 3] [-3 3]]]
    (split-axis 1 a b)))

(comment
  (let [a [[-10 10] [-10 10] [-10 10]]
        b [[11 15] [-3 3] [-3 3]]]
    (outside? a b)))

(comment
  (let [a [[-10 10] [-10 10] [-10 10]]
        b [[-3 3] [-3 3] [-3 3]]]
    (split a b))
  (let [a [[-10 10] [-10 10] [-10 10]]
        b [[-13 13] [-13 3] [-13 13]]]
    (split a b))
  )
