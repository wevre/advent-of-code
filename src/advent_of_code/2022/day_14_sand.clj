(ns advent-of-code.2022.day-14-sand
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]))

;; 2022-12-13 10:56
;;    So sloppy and crude, but I don't care. I got the stars and now I can go
;;    and get some rest for snowboarding tomorrow!

(defn get-rocks [input]
  (let [get-points (fn [lines]
                     (reduce (fn [acc [a b]]
                               (into acc (common/points-along a b)))
                             #{}
                             lines))]
    (->> input
         str/split-lines
         (map common/parse-longs)
         (map #(partition 2 %))
         (map #(partition 2 1 %))
         (map get-points)
         (reduce into #{}))))

(defn- blocked? [pos rocks sand]
  (or (rocks pos) (sand pos)))

(defn- floored [floor]
  (fn [[_ y :as pos] rocks sand]
    (or (= y floor) (blocked? pos rocks sand))))

(def start [500 0])

(def ^:dynamic *debug* false)

(defn cascade [lowest rocks block-fn]
  (fn [n [x y :as pos] sand]
    (when *debug* (println "try" pos "sand is" sand))
    (cond

      (and *debug* (neg? n)) sand

      (sand start) sand

      (> y lowest) sand

      (block-fn [x (inc y)] rocks sand)
      (cond
        (not (block-fn [(dec x) (inc y)] rocks sand)) (do (when *debug* (println "try left")) (recur (dec n) [(dec x) (inc y)] sand))
        (not (block-fn [(inc x) (inc y)] rocks sand)) (do (when *debug* (println "try right")) (recur (dec n) [(inc x) (inc y)] sand))
        :else (do (when *debug* (println "rest at" pos)) (recur (dec n) start (conj sand pos))))

      :else (recur (dec n) [x (inc y)] sand)
      ))
  )


(comment
  ;; puzzle 1
  (let [#_#_input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
        input (slurp "input/2022/14-sand.txt")
        rocks (get-rocks input)
        lowest (reduce max (map second rocks))
        sand ((cascade lowest rocks blocked?) 200 start #{})]
    (count sand)
    )

  ;; puzzle 2
  (binding [*debug* false]
    (let [#_#_input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
          input (slurp "input/2022/14-sand.txt")
          rocks (get-rocks input)
          lowest (reduce max (map second rocks))
          sand ((cascade (+ lowest 2) rocks (floored (+ lowest 2))) 1000 start #{})]
      (count sand)))

  )
