(ns advent-of-code.2023.day-07-camel-poker
  (:require [clojure.string :as str]))

(defn parse-line [l]
  (let [[h b] (str/split l #" ")]
    [h (parse-long b)]))

(def card-strength {\A 14 \K 13 \Q 12 \J 11 \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2})

(def j-card-strength (assoc card-strength \J 1))

(def type-strength {:five 50 :four 40 :house 32 :three 30 :two-pair 22 :one-pair 20 :high-card 10})

(defn type<- [hand]
  (let [fr's (frequencies hand)
        cnt (count fr's)]
    (condp = cnt
      5 :high-card
      4 :one-pair
      1 :five
      (condp = (-> fr's vals set)
        #{4 1} :four
        #{3 2} :house
        #{3 1} :three
        #{2 1} :two-pair))))

(defn j-type<- [hand]
  (let [fr's (frequencies hand)
        cnt (count fr's)]
    (condp = cnt
      5 (if (get fr's \J) :one-pair :high-card)
      4 (if (get fr's \J) :three :one-pair)
      1 :five
      (condp = (-> fr's vals set)
        #{4 1} (if (get fr's \J) :five :four)
        #{3 2} (if (get fr's \J) :five :house)
        #{3 1} (if (get fr's \J) :four :three)
        #{2 1} (condp = (get fr's \J 0)
                 0 :two-pair
                 1 :house
                 2 :four)))))

(defn comp-hand [type-er strength-er]
  (fn [ha hb]
    (let [c (compare (type-strength (type-er ha)) (type-strength (type-er hb)))]
      (if (not= c 0)
        c
        (compare (mapv strength-er ha) (mapv strength-er hb))))))

(defn solve [input comp]
  (->> input
       str/split-lines
       (map parse-line)
       (sort-by first comp)
       (map-indexed vector)
       (map (fn [[rank [_ bid]]] (* (inc rank) bid)))
       (reduce +)))

(comment
  (def input (slurp "input/2023/07-sample.txt"))
  (def input (slurp "input/2023/07-camel-poker.txt"))

  ;; year 2023 day 07 puzzle 1
  (solve input (comp-hand type<- card-strength))   ;; => 251136060

  ;; year 2023 day 07 puzzle 2
  (solve input (comp-hand j-type<- j-card-strength))   ;; => 249400220
)
