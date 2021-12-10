(ns advent-of-code.2021.day-6)

;; --- Day 6: Lanternfish ---

;; Ideas: we could memoize a function (defn get-count (gen age))
;; that comes back with how many fish there are after `gen` generations, based
;; on the fish's starting `age`.
;; For example: for a given fish the answer would be:
;; (if (> gen age) 
;;   (+ (get-count (- gen age 1) 6) (get-count (- gen age 1) 8))
;;   1)

(def get-count
  (memoize (fn [gen age]
             (if (> gen age)
               (+ (get-count (- gen age 1) 6) (get-count (- gen age 1) 8))
               1))))
(comment
  (let [input "3,4,3,1,2"]
    (->> input
         (re-seq #"\d")
         (map #(Integer/parseInt %))
         (map #(get-count 80 %))
         (apply +)))
  (get-count 5 3))

(defn puzzle [gens file]
  (->> (slurp file)
       (re-seq #"\d")
       (map #(Integer/parseInt %))
       (map #(get-count gens %))
       (apply +)))

(comment
  (puzzle 80 "input/2021/6-fish.txt")
  (puzzle 256 "input/2021/6-fish.txt"))