(ns advent-of-code.2016.day-15-timing
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.remainders :as remainders]))

(defn solve [input]
  (->> input
       common/parse-longs
       (partition 4)
       (map (fn [[i n _ c]] [n (mod (- n c i) n)]))
       sort
       reverse
       remainders/congruences))

(comment
  ;; puzzle 1
  (solve (slurp "input/2016/15-capsule-timing.txt"))   ; => 203660

  ;; puzzle 2
  (solve (str (slurp "input/2016/15-capsule-timing.txt") "\n7 11 0 0"))   ; => 2408135
  )

;; NOTE
;;
;; What is that (mod (- n c i) n) doing there?. Well, take the first disk, which
;; is 10 (mod 13). It takes i=1 second for the capsule to reach this, so the
;; amount of time I need to wait to drop the capsule here is
;;    n - c - i = 13 - 10 - 1 = 2 (mod 13)
;; and so on for all the disks.