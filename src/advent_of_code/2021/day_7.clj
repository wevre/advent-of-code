(ns advent-of-code.2021.day-7)

;; This was really hacky, I just poked around with the input list in the REPL
;; and didn't really write a "puzzle" function.

;; find the median, then find the cost for everyone to be moved to that.

(comment
    
  ;; find out how many
  (->> (slurp "input/2021/7-crabs.txt")
       (re-seq #"\d+")
       #_count)   ;;=> 1000
  
  ;; find the median
  (->> (slurp "input/2021/7-crabs.txt")
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       sort
       (drop 499)
       (take 2))   ;;=> (362 362)
  )

(defn cost [mid]
  (->>
   (slurp "input/2021/7-crabs.txt")
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (map #(Math/abs (- mid %)))
   (apply +)))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (pprint (map #(str "mid is " % " cost is " (cost %)) '(361 362 363)))
  )

;; The cost for part 2 leads towards the mean as the best point, since the cost
;; is now essentially the distance squared, so we want to find the least square.
(defn cost-2 [mid]
  (->>
   (slurp "input/2021/7-crabs.txt")
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (map #(Math/abs (- mid %)))
   (map #(/ (* % (inc %)) 2))
   (apply +)))

(comment
  ;; find the sum
  (->> (slurp "input/2021/7-crabs.txt")
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (apply +))   ;;=>499568
  
  ;; so the average is 500, but it turns out 499 has lower cost.
  (pprint (map #(str "mid is " % " cost is " (cost-2 %)) '(498 499 500 501 502)))

  )
