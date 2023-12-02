(ns advent-of-code.2023.day-02-games
  (:require [clojure.string :as str]))

(defn parse-info [s]
  (let [[game-id & info] (re-seq #"\d+|blue|green|red" s)]
    (reduce (fn [m [n k]] (update m (keyword k) max (parse-long n)))
            {:blue 0 :green 0 :red 0 :game (parse-long game-id)}
            (partition 2 info))))

(defn ?possible [limits]
  (fn [game-info] (->> (merge-with - limits game-info) vals (not-any? neg?))))

(defn power [{:keys [blue green red]}] (* blue green red))

(defn games<- [input] (->> input slurp str/split-lines (map parse-info)))

(comment
  (def games (games<- "input/2023/02-games.txt"))
  (def games (games<- "input/2023/02-sample-games.txt"))

  ;; year 2023 day 01 puzzle 1
  (let [xform (comp (filter (?possible {:blue 14 :green 13 :red 12})) (map :game))]
    (transduce xform + games))   ;; => 1853

  ;; year 2023 day 01 puzzle 2
  (transduce (map power) + games)   ;; => 72706
)
