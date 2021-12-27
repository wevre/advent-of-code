(ns advent-of-code.2015.day-03
  (:require [clojure.set]))

;;;; --- Day 3: Perfectly Spherical Houses in a Vacuum ---
;;;; https://adventofcode.com/2015/day/3

(def move-map {\^ [0 1] \> [1 0] \< [-1 0] \v [0 -1]})

(defn visited-once [moves locs]
  (loop [[move & moves] moves
         locs (into clojure.lang.PersistentQueue/EMPTY locs)
         houses (into #{} locs)]
    (if move
      (let [[loc locs] ((juxt peek pop) locs)
            loc (mapv + loc (move-map move))]
        (recur moves (conj locs loc) (conj houses loc)))
      houses)))

(comment

  (let [input ">"] (count (visited-once input [[0 0]])))
  (let [input "^v^v^v^v^v"] (count (visited-once input [[0 0]])))
  (let [input "^>v<"] (println #_count (visited-once input [[0 0]])))

  ;; part 1
  (let [input (slurp "input/2015/3-houses.txt")]
   (count (visited-once input [[0 0]])))   ;=> 2565

  ;; part 2
  (let [input (slurp "input/2015/3-houses.txt")]
    (count (visited-once input [[0 0] [0 0]])))   ;=> 2639
  )
