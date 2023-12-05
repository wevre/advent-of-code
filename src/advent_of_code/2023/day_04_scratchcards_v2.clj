(ns advent-of-code.2023.day-04-scratchcards-v2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

;; This incorporates some great ideas from Clojurians slack channel:
;; - reverse processing in `counts-rf` from @Sam Ferrell
;; - regex magic from @tschady

(defn counts-rf
  ([xs] (reduce + xs))
  ([xs x] (conj xs (reduce + 1 (take x xs)))))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(re-seq #"(?<=:.*)(?=\b(\d+)\b.*\|.*\b\1\b)" %))
       (map count)))

(defn points [wins] (if-not (pos? wins) 0 (long (math/pow 2 (dec wins)))))

(comment
  (def wins (parse (slurp "input/2023/04-sample-scratchcards.txt")))
  (def wins (parse (slurp "input/2023/04-scratchcards.txt")))

  ;; year 2023 day 04 puzzle 1
  (transduce (map points) + wins)   ;; => 15205

  ;; year 2023 day 04 puzzle 2
  (time
   (transduce identity counts-rf () (reverse wins)))   ;; => 6189740
  )
