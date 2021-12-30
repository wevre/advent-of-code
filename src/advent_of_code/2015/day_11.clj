(ns advent-of-code.2015.day-11)

;;;; --- Day 11: Corporate Policy ---
;;;; https://adventofcode.com/2015/day/11

(defn inc-pswd [p]
  (persistent!
   (loop [i (dec (count p)) p (transient p)]
     (case (p i)
       122 (recur (dec i) (assoc! p i 97))
       (104 107 110) (assoc! p i (+ 2 (p i)))
       (assoc! p i (inc (p i)))))))

(defn num-pairs [p]
  (->> (partition-by identity p)
       (keep #(when (<= 2 (count %)) (first %)))
       set
       count))

(defn straight? [p]
  (some (fn [[a b c]] (= 1 (- b a) (- c b))) (partition 3 1 p)))

(defn valid-pswd? [p]
  (and
   (not-any? #{105 108 110} p)
   (< 1 (num-pairs p))
   (straight? p)))

(defn next-password [input]
  (->> (mapv int input)
       (iterate inc-pswd)
       (drop 1)
       (filter valid-pswd?)
       first
       (map char)
       (apply str)))

(comment
  ;; part 1 -- 930ms
  (time
   (next-password "hepxcrrq"))  ;=>"hepxxyzz"

  ;; part 2 -- 2.2s
  (time
   (next-password "hepxxyzz"))  ;=>"heqaabcc"
  )
