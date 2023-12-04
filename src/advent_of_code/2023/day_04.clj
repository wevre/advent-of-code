(ns advent-of-code.2023.day-04
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn card-info [n]
  (fn [line]
    (let [[card & numbers] (re-seq #"\d+" line)
          [winning have] (split-at n numbers)]
      {:card (parse-long card) :wins (count (keep (set winning) have))})))

(defn count-em-up [cards]
  (loop [[{:as c :keys [card wins]} & c's] cards counts {1 0}]
    (if-not c
      counts
      (let [n (inc (get counts card 0))]
        (recur c's (reduce (fn [counts i] (update counts i (fnil + 0) n))
                           (update counts card (fnil inc 0))
                           (->> (range) (drop (inc card)) (take wins))))))))

(defn parse [n input]
  (map (card-info n) (str/split-lines input)))

(comment
  (def cards (parse 5 (slurp "input/2023/04-sample-scratchcards.txt")))
  (def cards (parse 10 (slurp "input/2023/04-scratchcards.txt")))

  ;; year 2023 day 04 puzzle 1
  (->> cards
       (map :wins)
       (filter pos?)
       (map #(long (math/pow 2 (dec %))))
       (reduce +))   ;; => 15205

  ;; year 2023 day 04 puzzle 2
  (time
   (->> (count-em-up cards)
        vals
        (reduce +)))   ;; => 6189740
  )