(ns advent-of-code.2016.day-14-one-time-pad
  (:require [clj-commons.digest :refer [md5]]))

(def ^:dynamic *salt* "qzyelonm")
(def ^:dynamic *limit* 30000)
(def ^:dynamic *stretch* 1)

(defn repeats [s n]
  (->> s
       (partition n 1)
       (keep #(when (apply = %) (first %)))))

(defn find-keys [n]
  (loop [index 0 found {} confirmed #{}]
    (if (or (and *limit* (>= index *limit*))
            (and (>= (count confirmed) n)
                 (> index (+ 1000 (nth (sort confirmed) (dec n))))))   ; <2>
      confirmed
      (let [hash (-> (iterate md5 (str *salt* index)) (nth *stretch*))
            found (if-let [c (first (repeats hash 3))]
                    (update found c (fnil conj #{}) index)
                    found)
            confirmed (reduce (fn [m c]
                                (->> (found c)
                                     (filter #(<= 1 (- index %) 1000))   ; <3>
                                     (into m)))
                              confirmed
                              (repeats hash 5))]
        (recur (inc index) found confirmed)))))

(comment
  ;; puzzle 1
  (-> (find-keys 64) sort (nth 63))   ; => 15168

  ;; puzzle 2
  (binding [*stretch* 2017]
    (-> (find-keys 64) sort (nth 63)))   ; => 20864
  )

;; NOTES:
;;
;; I ran into three bugs/issues while working on this code.
;;
;; 1. I didn't read carefully in the problem statement that when identifying a
;;    potential key, only consider the first triple. My original version was
;;    capturing too many potential keys. Only considering the first actually
;;    makes the code simpler.
;;
;; 2. This didn't occur on the sample problem, but did on my problem input. I
;;    stopped searching after confirming the 64th key, but there was a potential
;;    key, just a few indices behind, that would become confirmed and slip into
;;    the 64th spot if continued looking for quintuplets. Added logic to make
;;    sure and continue running confirmations even after finding 64th key.
;;
;; 3. Inside the loop I first update `found` and then `confirmed` but if a hash
;;    contains a quintuplet, I make the mistake of confirming the same hash just
;;    added to `found`. I added the lower bound of 1 when checking for
;;    differences in indices, but I could have also swapped the order and done
;;    the confirmations first and then update `found`.

(comment
  ;; example
  (binding [*salt* "abc" *stretch* 1]
    (-> (find-keys 64) sort (nth 63)))   ; => 22728 (part 1), 22551 (stretching)

  (-> (iterate md5 (str "abc" 89)) (nth 2017))
  )
