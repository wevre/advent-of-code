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
                 (> index (+ 1000 (nth (sort confirmed) (dec n))))))   ; <A>
      confirmed
      (let [hash (-> (iterate md5 (str *salt* index)) (nth *stretch*))
            found (if-let [c (first (repeats hash 3))]
                    (update found c (fnil conj #{}) index)
                    found)
            confirmed (reduce (fn [m c]
                                (->> (found c)
                                     (filter #(<= 1 (- index %) 1000))
                                     (into m)))
                              confirmed
                              (repeats hash 5))]
        (recur (inc index) found confirmed)))))

;; NOTES:
;; line <A> is there because you could find a triplet at, say, index 180 and
;; another one at index 200. But you might confirm index 200 before you confirm
;; index 180. If you stop after confirming 200, but before you've confirmed 180,
;; you'll report wrong results. Need to go a little bit extra to make sure we
;; don't have any pending confirmations.

(comment
  ;; puzzle 1
  (-> (find-keys 64) sort (nth 63))   ; => 15168

  ;; puzzle 2
  (binding [*stretch* 2017]
    (-> (find-keys 64) sort (nth 63)))   ; => 20864

  ;; example
  (binding [*salt* "abc" *stretch* 2017]
    (-> (find-keys 64) sort (nth 63)))   ; => 22728 (part 1), 22551 (stretching)

  (-> (iterate md5 (str "abc" 89)) (nth 2017))
  )
