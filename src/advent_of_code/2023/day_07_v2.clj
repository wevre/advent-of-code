(ns advent-of-code.2023.day-07-v2
  (:require [clojure.string :as str]))

(def strength {:five 50 :four 40 :house 32 :three 30 :two-pair 22 :one-pair 20 :high-card 10})

;; Remap upper cards so they sort correctly by card strength.
(def escapes {\A \E \K \D \Q \C \J \B \T \A})
(def j-escapes (assoc escapes \J \1))

(defn type<- [hand]
  (let [fr's (frequencies hand)
        cnt (count fr's)
        ?best (get fr's \1)]
    (case cnt
      5 (if ?best :one-pair :high-card)
      4 (if ?best :three :one-pair)
      1 :five
      (case (-> fr's vals set)
        #{4 1} (if ?best :five :four)
        #{3 2} (if ?best :five :house)
        #{3 1} (if ?best :four :three)
        #{2 1} (case (get fr's \1 0) 0 :two-pair 1 :house 2 :four)))))

(defn parse-line [escape-er]
  (fn [l]
    (let [[cards bid] (str/split l #" ")
          sortable (str/escape cards escape-er)
          type (type<- sortable)]
      ;; The vector [strength sortable type cards bid] will sort/rank correctly.
      ;; But it also keeps human-readable info for debugging.
      [(strength type) sortable type cards (parse-long bid)])))

(defn solve [input escape-er]
  (->> input
       str/split-lines
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
