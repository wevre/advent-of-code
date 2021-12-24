(ns advent-of-code.2015.day-20)

;;;; --- Day 20: Infinite Elves and Infinite Houses ---
;;;; https://adventofcode.com/2015/day/20

;;; Use a really large array and deliver presents brute force.

;; Not as elegant of code. Uses mutable java array with side-effects. Using `do`
;; and `doseq` is typically a code smell. At least the mutations are all
;; self-contained within the `solve` function.

(def house-count 1000000)
(def target 33100000)

(defn solve [elf-limit per-elf]
  (let [houses (long-array house-count 10)
        elf-range (if elf-limit (range elf-limit) (range))]
    (loop [elf 2]
      (if (< elf house-count)
        (do
          (doseq [house (map #(* (inc %) elf) elf-range)
                  :while (< house house-count)
                  :let [val (+ (aget houses (dec house)) (* per-elf elf))]]
            (aset houses (dec house) val))
          (if (<= target (aget houses (dec elf)))
            elf
            (recur (inc elf))))
        :fail))))

(comment
  ;; part 1 -- ~85s
  (time
   (solve nil 10))   ; 776160

  ;; part 2 -- ~27s
  (time
   (solve 50 11))   ; 786240
  )

;;; Use Euler's recurrence for sums of divisors, with pentagonal numbers.

;; I love this code, it is very math-nerd-ical. But it is also slow.

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
  ;; part 1  -- over 9 minutes
  (time
   (->> (map sigma (range))
        (take-while #(< % 3310000))
        count))
  )
