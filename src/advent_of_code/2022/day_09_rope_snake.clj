(ns advent-of-code.2022.day-09-rope-snake
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ∆s<-d {'U [+1 0] 'L [0 -1] 'D [-1 0] 'R [0 +1]})

(defn signum [x] (cond (zero? x) 0 (pos? x) +1 :else -1))

(defn pull [[hr hc] [tr tc :as t]]
  (let [∆r (- hr tr) ∆c (- hc tc)]
    (cond
      (and (<= -1 ∆r +1) (<= -1 ∆c +1)) t
      :else [(+ tr (signum ∆r)) (+ tc (signum ∆c))])))

(comment
  (pull [1 4] [0 2])   ; => [1 3]
  )

(defn move [[head & tail] ∆]
  (reduce (fn [rope knot] (conj rope (pull (peek rope) knot)))
          [(map + head ∆)]
          tail))

(defn puzzle [n input]
  (->> input
       str/split-lines
       (map #(str "[" % "]"))
       (map edn/read-string)
       (mapcat (fn [[d n]] (repeat n (∆s<-d d))))
       (reductions move (repeat n [0 0]))
       (map last)
       distinct
       count))

(comment
  ;; puzzle 1
  (puzzle 2 (slurp "input/2022/09-rope-snake.txt"))   ; => 5683

  ;; puzzle 2
  (puzzle 10 (slurp "input/2022/09-rope-snake.txt"))   ; => 2372
  )
