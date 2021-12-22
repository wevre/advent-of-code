(ns advent-of-code.2015.day-20)

;;; Using Euler's recurrence for sums of divisors, with pentagonal numbers.

;; I love this code, it is very math-nerd-ical. But it is slow.

(defn make-second-order [x d]
  (map first (iterate (fn [[a b]] [(+ a b) (+ b d)]) [x (+ x d)])))

(def pentagonal (interleave (make-second-order 1 3) (make-second-order 2 3)))

(def sigma
 (memoize
  (fn [n]
    (->>
     (map #(cond (< n %) 0 (= n %) n :else (sigma (- n %))) pentagonal)
     (take-while pos?)
     (map * (cycle [+1 +1 -1 -1]))
     (reduce +))))
  )

(comment
  ;; part 1  -- takes over 9 minutes
  (time
   (->> (map sigma (range))
        (take-while #(< % 3310000))
        count))
  )

;;; Using a really large array and loading up presents brute force.

(comment
  ;; part 1 -- takes 2.5 minutes
  (do
    (def count 1000000)
    (def houses (long-array count 10))
    (def target 33100000)
    (time
     (loop [elf 2]
       (if (< elf count)
         (do
           (doseq [house (map #(* (inc %) elf) (range))
                   :while (< house count)
                   :let [val (+ (aget houses (dec house)) (* 10 elf))]]
             (aset houses (dec house) val))
           (if (<= target (aget houses (dec elf)))
             elf
             (recur (inc elf))))
         :fail))))

  ;; part 2 -- takes 1.5 seconds
  (do
    (def count 2000000)
    (def houses (long-array count 10))
    (def target 33100000)
    (time
     (loop [elf 2]
       (if (< elf count)
         (do
           (doseq [house (map #(* % elf) (range 1 51))
                   :when (< house count)
                   :let [val (+ (aget houses (dec house)) (* 11 elf))]]
             (aset houses (dec house) val))
           (if (<= target (aget houses (dec elf)))
             elf
             (recur (inc elf))))
         :fail))))
  )
