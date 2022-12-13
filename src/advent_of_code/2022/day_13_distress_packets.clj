(ns advent-of-code.2022.day-13-distress-packets
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]))

;; 2022-12-12 10:46
;;    Initial, crude solutions.
;; 2022-12-12 11:54
;;    Still not convinced that my comparator is as simple as it can be, but I
;;    did improve parsing somewhat. It is a legit comparator and just like the
;;    docs say, those are tricky to get right.
;; 2022-12-13 00:48
;;    Don't need to `map-index` followed by `keep`, just use `keep-indexed`.
;;    Also, my comparison of the `first` of a and b followed by comparison of
;;    `rest` of a and b can be accomplished with `map`ping comparison over a and
;;    b and searching the resulting sequence for first non-zero value. Saw that
;;    in a few others solutions which reminded me that I had done the same
;;    approach in a natural sort comparator I wrote a couple of years ago
;;    (https://github.com/wevre/natural-compare). I actually played around for a
;;    while with flattened input, using another trick from that natural sort
;;    where I pad the end with -1 so that shorter vectors sort first. But the
;;    empty vectors just don't play nice, so I don't think there is a way to get
;;    around descending into each packet.
;; 2022-12-13 01:03
;;    @nbardiuk shared a good insight: don't have to split lines and read-string
;;    each line, just wrap the entire input inside [] and read it in.
;; 2022-12-13 09:07
;;    Thought I had figured out a way to normalize/flatten and do a more
;;    straight-forward elementwise compare, but alas, I think my observation
;;    above still stands that just can't flatten away some of the more intricate
;;    nesting. I'm going to commit it, and then remove it and re-commit. Along
;;    the way, I incoporated one of my natural-sort tricks, conj -inf onto the
;;    end of the vector, that let me simplify the comparator.

(def ^:dynamic *debug* false)

(defn packet-compare [a b]
  (when *debug* (println "comparing" a "to" b))
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) (recur [a ##-Inf] b)
    (number? b) (recur a [b ##-Inf])
    :else (->> (map packet-compare a b) (drop-while zero?) first)))

(defn parse [input]
  (edn/read-string (str "[" input "]")))

(def dividers #{[[2]] [[6]]})

;; An attempt (failed) to normalize/flatten followed by elementwise compare.
(defn txf [a]
  (cond
    (= [] a) nil
    (vector? a) (conj a ##-Inf)
    :else a))

(defn normalize [p]
  (flatten [(walk/postwalk txf p) nil]))

(defn vec-compare [a b]
  (->> (map compare a b) (drop-while zero?) first))

(comment
  ;; all of these are -1, meaning for each [a b], a < b
  (map #(apply compare %) [[-1 0] [nil 0] [##-Inf -1] [nil ##-Inf]])
  )

(comment
  (binding [*debug* true]
    (let [a [[[3 6 10] [[10 4 4] [9 0] 10 []] [9 [] [5 4 3 6 6]] 6] [] [] [4 3 4 10] [[[10]] 0]]
          b [[[[3 0] [6 2 8]] [[3] 5 [] [10 8 7 6]] [9 6]] []]]
      (packet-compare a b)))

  (binding [*debug* true]
    (let [a [[[3 6 10] [[10 4 4] [9 0] 10 []] [9 [] [5 4 3 6 6]] 6] [] [] [4 3 4 10] [[[10]] 0]]
          b [[[[3 0] [6 2 8]] [[3] 5 [] [10 8 7 6]] [9 6]] []]]
      (map vector (normalize a) (normalize b))))
  )

(comment
  ;; tricky test case
  (let [orig (->> (parse (slurp "input/2022/13-distress-packets.txt"))
                  (partition 2))
        cmp1 (->> orig
                  (map #(apply packet-compare %)))
        cmp2 (->> (parse (slurp "input/2022/13-distress-packets.txt"))
                  (map #(walk/postwalk txf %))
                  (map #(flatten [% nil]))
                  (partition 2)
                  (map (fn [[a b]] (->> (map compare a b) (drop-while zero?) first))))]
    (->> (map vector orig cmp1 cmp2)
         (filter (fn [[_ c1 c2]] (or (< c1 0 c2) (< c2 0 c1)))))
    )

  ;; alt puzzle 1 -- not right answer
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (map #(walk/postwalk txf %))
       (map #(flatten [% nil]))
       (partition 2)
       (map #(apply vec-compare %))
       (keep-indexed (fn [i c] (when (< c 0) (inc i))))
       (apply +))


  ;; puzzle 1
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (partition 2)
       (map #(apply packet-compare %))
       (keep-indexed (fn [i c] (when (< c 0) (inc i))))
       (apply +))   ; => 5905

  ;; puzzle 2
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (concat dividers)
       (sort packet-compare)
       #_#_
       (keep-indexed (fn [i p] (when (dividers p) (inc i))))
       (apply *))   ; => 21691
  )
