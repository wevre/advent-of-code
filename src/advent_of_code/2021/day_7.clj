(ns advent-of-code.2021.day-7)

;; --- Day 7: The Treachery of Whales ---

;; I didn't write typical functions here, because I didn't want to calculate the
;; cost for _every_ position, just needed to find the median/mean and then test
;; enough numbers nearby to make sure it was correct.

(def input (->> (slurp "input/2021/7-crabs.txt")
                (re-seq #"\d+")
                (map #(Integer/parseInt %))))
(comment
  ;; find out how many
  (count input)   ;;=> 1000
  ;; find the median
  (->> input sort (drop 499) (take 2))   ;;=> (362 362)
  )

(defn cost-1 [mid]
  (->>
   input
   (map #(- mid %))
   (map #(Math/abs %))
   (apply +)))

(comment
  (let [test '(361 362 363)]
    (->> test
         (map #(vector (cost-1 %) %))
         (sort-by first)
         first))   ;;=> [355150 362]
  )

;; The cost for part 2 is a squared distance, which hints towards the mean.
(defn cost-2 [mid]
  (->>
   input
   (map #(- mid %))
   (map #(/ (* % (inc %)) 2))   ;; Sum from 1 to n is n*(n+1)/2.
   (apply +)))

(comment
  ;; find the sum
  (->> input (apply +))   ;;=> 499568

  ;; mean is between 499 and 500, it turns out 499 has lower cost.
  (let [test '(498 499 500 501)]
    (->> test
         (map #(vector (cost-2 %) %))
         (sort-by first)
         first))   ;;=> [98368490 499]
  )
