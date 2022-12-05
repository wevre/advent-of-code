(ns advent-of-code.2022.day-05-cranes
  (:require [advent-of-code.common :as common]
            [advent-of-code.common.file :as file]))

(defn parse-drawing
  "Return a map of stack num to (seq of) crates."
  [lines]
  (let [width (transduce (map count) max 0 lines)
        pad (fn [s] (format (str "%-" width "s") s))]
    (->> lines
         (map pad)
         (map #(transduce (comp (drop 1) (take-nth 4)) conj %))
         drop-last
         (apply map vector)
         (map reverse)
         (map #(remove #{\space} %))
         (map vector (drop 1 (range)))
         (into {}))))

(defn parse
  "Parses stack drawing then applies moves, using sort-fn to simulate
   CrateMover 9000 (reverse) or CrateMover 9001 (identity)."
  [sort-fn input]
  (let [[stacks moves] (file/split-grouped-lines input)
        stacks (parse-drawing stacks)
        moves (map common/parse-longs moves)]
    (reduce (fn [stacks [n from to]]
              (-> stacks
                  (update to concat (sort-fn (take-last n (get stacks from))))
                  (update from (partial drop-last n))))
            stacks
            moves)))

(defn topmost [ss]
  (transduce (comp (map second) (map last)) str (sort ss)))

(comment
  ;; puzzle 1
  (topmost (parse reverse (slurp "input/2022/05-cranes.txt")))   ; => SHMSDGZVC

  ;; puzzle 2
  (topmost (parse identity (slurp "input/2022/05-cranes.txt")))   ; => VRZGHDFBQ
  )
