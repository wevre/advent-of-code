(ns advent-of-code.2022.day-02-rock-paper-scissors
  (:require [clojure.string :as str]))

(defn puzzle [strategy input]
  (->> input str/split-lines (map strategy) (map vals) flatten (reduce +)))

(def rock 1)
(def paper 2)
(def scissors 3)
(def lose 0)
(def draw 3)
(def win 6)

(def strategy1 {"A X" {:shape rock     :outcome draw}
                "A Y" {:shape paper    :outcome win}
                "A Z" {:shape scissors :outcome lose}
                "B X" {:shape rock     :outcome lose}
                "B Y" {:shape paper    :outcome draw}
                "B Z" {:shape scissors :outcome win}
                "C X" {:shape rock     :outcome win}
                "C Y" {:shape paper    :outcome lose}
                "C Z" {:shape scissors :outcome draw}})

(def strategy2 {"A X" {:shape scissors :outcome lose}
                "A Y" {:shape rock     :outcome draw}
                "A Z" {:shape paper    :outcome win}
                "B X" {:shape rock     :outcome lose}
                "B Y" {:shape paper    :outcome draw}
                "B Z" {:shape scissors :outcome win}
                "C X" {:shape paper    :outcome lose}
                "C Y" {:shape scissors :outcome draw}
                "C Z" {:shape rock     :outcome win}})

(comment
  (puzzle strategy1 (slurp "input/2022/02-rock-paper-scissors.txt"))   ;=> 15632

  (puzzle strategy2 (slurp "input/2022/02-rock-paper-scissors.txt"))   ;=> 14416

  (puzzle strategy2 "A Y\nB X\nC Z")

  )
