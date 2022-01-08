(ns advent-of-code.2015.day-11)

;;;; --- Day 11: Corporate Policy ---
;;;; https://adventofcode.com/2015/day/11

(defn next-paswd [s]
  (if (= \z (last s))
    (apply str (concat (next-paswd (butlast s)) '(\a)))
    (apply str (concat (butlast s) (-> s last int inc char list)))))

(defn num-pairs [p]
  (->> (partition-by identity p)
       (keep #(when (<= 2 (count %)) (first %)))
       set
       count))

(defn straight? [p]
  (some #(let [[a b c] (map int %)] (= (inc a) b (dec c))) (partition 3 1 p)))

(defn valid-pswd? [p]
  (and
   (not-any? #{\i \l \o} p)
   (< 1 (num-pairs p))
   (straight? p)))

(defn next-password [input]
  (->> (iterate next-paswd input)
       (drop 1)
       (filter valid-pswd?)
       first))

(comment
  ;; part 1 -- 1.8s
  (time
   (next-password "hepxcrrq"))  ;=>"hepxxyzz"

  ;; part 2 -- 4.5s
  (time
   (next-password "hepxxyzz"))  ;=>"heqaabcc"
  )
