(ns advent-of-code.2023.day-02-games
  (:require [clojure.string :as str]))

(defn parse-info [s]
  (let [[game-id & info] (re-seq #"\d+|blue|green|red" s)]
    (reduce (fn [m [n k]] (update m (keyword k) max (parse-long n)))
            {:blue 0 :green 0 :red 0 :game (parse-long game-id)}
            (partition 2 info))))

(defn ?possible [limits]
  (fn [game-info]
    (->> (merge-with - limits game-info) vals (not-any? neg?))))

(defn power [{:keys [blue green red]}] (* blue green red))

(comment
  (def lines (-> (slurp "input/2023/02-games.txt") str/split-lines))
  (def lines (-> (slurp "input/2023/02-sample-games.txt") str/split-lines))

  ;; year 2023 day 01 puzzle 1
  (transduce (comp (map parse-info)
                   (filter (?possible {:blue 14 :green 13 :red 12}))
                   (map :game))
             +
             lines)   ;; => 1853


  ;; year 2023 day 01 puzzle 2
  (transduce (comp (map parse-info) (map power)) + lines)   ;; => 72706
)
