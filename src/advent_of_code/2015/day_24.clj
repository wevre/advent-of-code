(ns advent-of-code.2015.day-24-v2
  (:require [clojure.edn :as edn]))

;; --- Day 24: It Hangs in the Balance ---

(defn entangled [targ xs]
  (let [valid-remain? (fn [xs] (or (empty? xs) (seq (entangled targ xs))))
        step
        (fn step [res acc n [f & r]]
          (cond
            (neg? n) res
            (< 0 (count res) (count acc)) res
            (zero? n) (if (and (or (empty? res)
                                   (< (count acc) (count res))
                                   (< (reduce * acc) (reduce * res)))
                               (valid-remain? (remove (set acc) xs)))
                        acc
                        res)
            (nil? f) res
            :else
            (let [res (step res (conj acc f) (- n f) r)]
              (recur res acc n r))))]
    (step () () targ xs)))

(defn puzzle [groups input]
  (let [packages (map edn/read-string (re-seq #"\d+" input))
        targ (/ (apply + packages) groups)
        res (entangled targ (reverse packages))]
    [(reduce * res) res]))

(comment
  (time (let [input (slurp "input/2015/24-packages.txt")]
          [(puzzle 3 input) (puzzle 4 input)]))

  (let [input "1\n2\n3\n4\n5\n7\n8\n9\n10\n11"]
    [(puzzle 3 input) (puzzle 4 input)]))

;; This approach follows an algorithm laid out by a reddit poster. I think it
;; is faster than the above (and probably clearer about what it is doing).
;; I also think could do a quick reduce before the `loop` and determine what the
;; smallest group size n is, then start loop at i=n rather than i=1.
(defn entangled
  "Find smallest subset with lowest entanglement such that remaining ps _also_
   can form a valid subset."
  [targ ps]
  (loop [i 1]
    (cond
      (empty? ps) ()
      (< (count ps) i) nil
      :else
      (if-let [r (->> (combo/combinations ps i)
                      (filter #(= targ (reduce + %)))
                      (sort-by #(reduce * %))
                      (filter #(entangled targ (remove (set %) ps)))
                      seq)]
        (first r)
        (recur (inc i))))))
