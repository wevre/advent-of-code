(ns advent-of-code.2023.day-18-trenches
  (:require [clojure.string :as str]))

(def U [-1 0])
(def D [1 0])
(def R [0 1])
(def L [0 -1])

(def dir's {"U" U, "D" D, "L" L, "R" R})

(defn parse [s]
  (let [[_ dir len _] (re-matches #"(R|L|U|D) (\d+) \(#([0-9a-f]+)\)" s)]
    [(dir's dir) (parse-long len)]))

(defn verts [input]
  (loop [loc [0 0] [[dir len :as l] & l's] input verts []]
    (if-not l
      verts
      (let [nxt (mapv + loc (map #(* % len) dir))]
        (recur nxt l's (conj verts nxt))))))

(defn area [verts]
  (/ (->> (concat verts (take 1 verts))
          (partition 2 1)
          (map (fn [[[sr sc] [er ec]]] (- (* sr ec) (* er sc))))
          (reduce +)
          abs)
     2))

(defn parse2 [s]
  (let [[_ _ _ col] (re-matches #"(R|L|U|D) (\d+) \(#([0-9a-f]+)\)" s)]
    [({\0 R \1 D \2 L \3 U} (last col)) (Long/parseLong (subs col 0 5) 16)])
  )

(comment
  (def lines (str/split-lines (slurp "input/2023/18-sample.txt")))
  (def lines (str/split-lines (slurp "input/2023/18-trenches.txt")))

  ;; year 2023 day 18 puzzle 1
  (let [input (map parse lines)
        verts (verts input)
        perim (transduce (map second) + input)]
    (+ (area verts) (inc (/ perim 2))))
  ;; => 52055

  ;; year 2023 day 18 puzzle 2
  (let [input (map parse2 lines)
        verts (verts input)
        perim (transduce (map second) + input)]
    (+ (area verts) (inc (/ perim 2))))
  ;; => 67622758357096
  )
