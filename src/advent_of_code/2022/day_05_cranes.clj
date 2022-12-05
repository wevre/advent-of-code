(ns advent-of-code.2022.day-05-cranes
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn parse-line [w]
  (fn [l]
    (let [spec (str "%-" w "s")]
      (transduce (comp (drop 1) (take-nth 4)) conj (format spec l)))))

(defn parse-drawing
  "Return a map of stack num to (seq of) crates."
  [lines]
  (let [width (transduce (map count) max 0 lines)]
    (->> lines
         drop-last
         reverse
         (map (parse-line width))
         (apply map vector)
         (map #(remove #{\space} %))
         (map vector (drop 1 (range)))
         (into {}))))

(defn parse
  "Parses stack drawing then applies moves, using sort-fn to simulate
   CrateMover 9000 (reverse) or CrateMover 9001 (identity)."
  [sort-fn input]
  (let [[stacks moves] (str/split input #"\n\n")
        stacks (parse-drawing (str/split stacks #"\n"))
        moves (->> moves str/split-lines (map common/parse-longs))]
    (reduce (fn [stacks [n from to]]
              (-> stacks
                  (update to concat (sort-fn (take-last n (get stacks from))))
                  (update from (partial drop-last n))))
            stacks
            moves)))

(defn topmost [ss]
  (transduce (comp (map inc) (map ss) (map last)) str (range (count ss))))

(comment
  ;; puzzle 1
  (topmost (parse reverse (slurp "input/2022/05-cranes.txt")))   ; => SHMSDGZVC

  ;; puzzle 2
  (topmost (parse identity (slurp "input/2022/05-cranes.txt")))   ; => VRZGHDFBQ
  )
