(ns advent-of-code.2022.day-13-distress-packets
  (:require [clojure.edn :as edn]))

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
;;    straight-forward element-wise compare, but alas, I think my observation
;;    above still stands that I just can't flatten away some of the more
;;    intricate nesting. I'm going to commit it, and then remove it and
;;    re-commit.

(defn packet-compare [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) (recur [a] b)
    (number? b) (recur a [b])
    :else (or (->> (map packet-compare a b) (drop-while zero?) first)
              (- (count a) (count b)))))

(defn parse [input]
  (edn/read-string (str "[" input "]")))

(def dividers #{[[2]] [[6]]})

(comment
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
       (keep-indexed (fn [i p] (when (dividers p) (inc i))))
       (apply *))   ; => 21691
  )
