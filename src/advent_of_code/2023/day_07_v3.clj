(ns advent-of-code.2023.day-07-v3
  (:require [clojure.string :as str]))

(def strength {:five 50 :four 40 :house 32 :three 30 :two-pair 22 :one-pair 20 :high-card 10})

(def type<- {[1 1 1 1 1] :high-card
             [2 1 1 1] :one-pair, [1 1 1 1 0] :one-pair
             [2 2 1] :two-pair
             [3 2] :house, [2 2 0] :house
             [3 1 1] :three, [1 1 1 0] :three, [2 1 1 0] :three
             [4 1] :four, [3 1 0] :four, [1 1 0] :four, [2 1 0] :four
             [5] :five, [3 0] :five, [2 0] :five, [4 0] :five, [1 0] :five, [0] :five})

(def escapes {\A \E \K \D \Q \C \J \B \T \A})
(def j-escapes (assoc escapes \J \1))

(defn sig<- [hand]
  (into [] (sort > (->> (frequencies hand) (map (fn [[k v]] (if (= k \1) 0 v)))))))

(defn parse-line [escape-er]
  (fn [l]
    (let [[cards bid] (str/split l #" ")
          sortable (str/escape cards escape-er)
          type (type<- (sig<- sortable))]
      [(strength type) sortable type cards (parse-long bid)])))

(defn solve [input escape-er]
  (->> (str/split-lines input)
       (map (parse-line escape-er))
       sort
       (map-indexed (fn [rank info] (* (inc rank) (last info))))
       (reduce +)))

(comment
  (def input (slurp "input/2023/07-sample.txt"))
  (def input (slurp "input/2023/07-camel-poker.txt"))

  ;; year 2023 day 07 puzzle 1
  (solve input escapes)   ;; => 251136060

  ;; year 2023 day 07 puzzle 2
  (solve input j-escapes)   ;; => 249400220
  )