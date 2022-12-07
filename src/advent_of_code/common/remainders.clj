(ns advent-of-code.common.remainders)

;; An simplified implementation of Gauss's algorithm for Chinese Remainder
;; Theorem.
;;
;; The idea is given numbers that are coprime in pairs (meaning take any two and
;; their gcd is 1), then there exists a numbers that solve
;;
;;    x === c_1 (mod n_1)
;;    x === c_2 (mod n_2)
;;      ...
;;    x === c_r (mod n_r)
;;
;; You read those as x = c (mod n) means all the numbers x that have a remainder
;; of c when dividing by n. For example,
;;
;;    2 (mod 5) === 7, 12, 17, ...
;;
;; Gauss's algorithm is let N=n_1*n_2*...*n_r then
;;    x === c_1*N_1*d_1 + c_2*N_2*d_2 + ... + c_r*N_r*d_r (mod N)
;; where N_i = N/n_i and d_i === N_i^-1 (mod n_i).
;;
;; A modular inverse, like d_i, and written d === e^-1 (mod n) means a number
;; e such that e*d = 1 mod n. In other words, we find a number d such that
;; product e*d has a remainder of 1 modulo n. It only exists if n and e are
;; coprime.
;;
;; You need an algorithm to find those d_i's. But a simpler approach is as
;; follows:
;;
;; Start with the largest modulus and write out numbers until you find one
;; congruent to the next. For example:
;;
;;    x === 1 (mod 3)
;;    x === 2 (mod 4)
;;    x === 3 (mod 5)
;;
;; Start with 3 (mod 5), searching 3, 8, 13, 18, ... until we find one congruent
;; with 2 (mod 4) (which is 18 === 3 (mod 5) === 2 (mod 4)). Now we progress
;; with 18 (mod 20) until we find a number congruent with 1 (mod 3)
;;   18 (mod 20) === 18, 38, 58, ...
;; which finds 58 === 18 (mod 20) === 1 (mod 3) === 58 (mod 60). So our answer
;; is 58.

(defn gcd [a b] (if (= 0 b) a (recur b (mod a b))))

(defn lcm [a b] (/ (* a b) (gcd a b)))

(defn congruent [[n1 a1] [n2 a2]]
  (if (= (mod a1 n2) a2)
    [(lcm n1 n2) a1]
    (recur [n1 (+ a1 n1)] [n2 a2])))

(defn congruences
  "Input is a sequence of vectors that represent congruent numbers:
   [n c] === c (mod n)"
  [ms]
  (second (reduce (fn [m1 m2] (congruent m1 m2)) ms)))
