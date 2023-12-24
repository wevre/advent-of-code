(ns advent-of-code.2023.day-24
  (:require [advent-of-code.common :as common]
            [clojure.math :refer [sqrt]]
            [clojure.math.combinatorics :as combo]))

(defn parse [input]
  (->> (common/parse-longs input)
       (partition 6)))

(comment
  (do
    (def input (parse (slurp "input/2023/24-sample.txt")))
    (def lo 7)
    (def hi 27))
  (do
    (def input (parse (slurp "input/2023/24-hailstones.txt")))
    (def lo 200000000000000)
    (def hi 400000000000000)))

(defn slope-intercept [[px py _pz vx vy _vz]]
  (let [slope (/ vy vx)]
    [slope (- py (* px slope))]))

(defn intersection [line1 line2]
  (let [[m1 b1] (slope-intercept line1)
        [m2 b2] (slope-intercept line2)]
    (when (not= m1 m2)
      (let [x (/ (- b2 b1) (- m1 m2))
            [px1 _ _ vx1] line1
            [px2 _ _ vx2] line2
            t1 (/ (- x px1) vx1)
            t2 (/ (- x px2) vx2)]
        [x (+ (* m1 x) b1) t1 t2]))))

(comment
  ;; year 2023 day 24 puzzle 1
  (->> (combo/combinations input 2)
       (keep (fn [[a b]] (intersection a b)))
       (filter (fn [[x y t1 t2]]
                 (and (<= lo x hi) (<= lo y hi)
                      (<= 0 t1) (<= 0 t2))))
       count)
  ;; => 16050
  )

(defn primes [n]
  (let [hi (sqrt n)]
    (loop [n n f 2 r []]
      (if (<= hi f)
        (cond-> r
          (< 2 n) (conj n))
        (if (zero? (rem n f))
          (recur (/ n f) f (conj r f))
          (recur n (if (= 2 f) 3 (+ 2 f)) r))))))

(defn common [l1 l2]
  (loop [[n & n's :as l1] l1 [m & m's :as l2] l2 acc []]
    (if-not (and n m)
      acc
      (cond
        (< n m) (recur n's l2 acc)
        (< m n) (recur l1 m's acc)
        :else (recur n's m's (conj acc n))))))

;; Take two hailstones, i and j, that have the same velocity in one coordinate.
;; So we have vi = vj = v. These stones with same velocity give us a clue to
;; the rock's velocity, vr: (vr - v) has to be a factor of (xi - xj). So we can
;; follow this algorithm to find vr:
;;    1. Find a group of stones with same velocity.
;;    2. Find all the pairwise delta's between positions: (xi - xj)
;;    3. Find the prime factorization of each delta.
;;    4. Find the common factors across all the prime factorizations.
;;    5. Generate all numbers, p, from the common prime factors.
;;       Since (vr - v) =? p, and we might have negative factors, we check both
;;       vr = v + p and vr = v - p.
;;    6. Generate (+ v p) and (- v p) for all p's.
;;    7. Take all the numbers from #6 for all the common velocity groups, and
;;       find the common number across all of them. There will be one. It is vr.

(defn find-rock-velocity [input pos vel]
  (->>
   (for [[v stones] (group-by #(nth % vel) input)
         :when (< 2 (count stones))]
     (let [position's (map #(nth % pos) stones)
           pair's (combo/combinations position's 2)
           delta's (map (fn [[a b]] (abs (- a b))) pair's)
           factors (map primes delta's)
           common-factors (reduce common factors)
           possible's (map #(reduce * %) (combo/subsets common-factors))]
       (sort (concat (map #(+ % v) possible's)
                     (map #(- v %) possible's)))))
   (reduce common)
   first))

(comment
  ;; year 2023 day 24 puzzle 2 (about 7s)
  ;; The following three give us the rock's velocity: u, v, w
  (time
   (do
     (def u (find-rock-velocity input 0 3)) ;; => 214
     (def v (find-rock-velocity input 1 4)) ;; => -168
     (def w (find-rock-velocity input 2 5)) ;; => 249
     ;; With these velocities in hand it is not too difficult algebra, using
     ;; equations based on the first 2 input hailstones, to find initial
     ;; positions x, y, z. Here is an equation that solves it:
     (let [stone1 [176253337504656, 321166281702430, 134367602892386, 190, 8, 338]
           stone2 [230532038994496, 112919194224200, 73640306314241, 98, 303, 398]
           [a b _c u1 v1 _w1] stone1
           [d e f u2 v2 w2] stone2
           s (/ (- (* (- e b) (- u1 u)) (* (- d a) (- v1 v)))
                (- (* (- v1 v) (- u2 u)) (* (- v2 v) (- u1 u))))
           x (+ d (* s (- u2 u)))
           y (+ e (* s (- v2 v)))
           z (+ f (* s (- w2 w)))]
       [x y z (+ x y z)])))
   ;; => [172543224455736 348373777394510 148125938782131 669042940632377]
  )
