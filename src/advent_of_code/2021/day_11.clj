(ns advent-of-code.2021.day-11
  (:require [advent-of-code.common :refer [locmap<-digits]]))

;; --- Day 11: Dumbo Octopus ---
;; https://adventofcode.com/2021/day/11

(def input (locmap<-digits (slurp "input/2021/11-octopi.txt")))

(defn neighbors [[r c]]
  (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
    [(+ r dr) (+ c dc)]))

(defn step
  "Takes an octopi map as input, along with a queue of locations to examine. The
   queue starts with every location on the grid, and as flashes occur neighbor
   locations will be thrown back onto the queue to be re-examined.
   Returns octopi map as output, with a value of 0 indicating a flash.
   During processing, any octopus that flashes gets replaced with :f, preventing
   us from flashing it more than once. On return, converts :f's back to 0's."
  ([omap] (step omap (keys omap)))
  ([omap queue]
   (if-let [loc (first queue)]
     (cond
       (= :f (omap loc)) (recur omap (next queue))
       (<= 9 (omap loc)) (recur (assoc omap loc :f)
                                (into (next queue)
                                      (->> (neighbors loc)
                                           (select-keys omap)
                                           keys)))
       :else (recur (update omap loc inc) (next queue)))
     (reduce-kv (fn [m k v] (assoc m k (if (= :f v) 0 v))) {} omap))))

(defn print-omap [dim omap]
  (doseq [r (range dim)]
    (doseq [c (range dim)]
      (print (omap [r c])))
    (println)))

(comment
  (->> (locmap<-digits "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")
       (iterate step)
       (drop 205)
       first
       (print-omap 10))

  ;; puzzle 1
  (time
   (->> input
        (iterate step)
        (drop 1)
        (take 100)
        (map #(count (keep #{0} (vals %))))
        (reduce +)))

  ;; puzzle 2
  (time
   (->> input
        (iterate step)
        (map-indexed vector)
        (drop-while #(not-every? zero? (vals (second %))))
        ffirst))
  )
