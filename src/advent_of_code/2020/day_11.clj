(ns advent-of-code.2020.day-11
  (:require [clojure.set :as s]
            [clojure.string :as str]))

(defn print-grid [seats rows cols occupied]
  (doseq [r (range rows)]
    (println (str/join (for [c (range cols)]
                         (cond
                           (occupied [r c]) \#
                           (seats [r c]) \L
                           :else \.))))))

(defn step [seats neighbors too-many occupied]
  (let [neighs (mapcat neighbors occupied)]
    (s/union
     ; keep occupied seats that had no neighbors
     (s/difference occupied (set neighs))
     ; drop seats that had too many neighbors
     (set (for [[loc n] (frequencies neighs)
                :when (and (< n too-many) (occupied loc))]
            loc))
     ; add empty seats that had no neighbors
     (s/difference seats occupied (set neighs)))))

(defn stabilize [stepper]
  (->> #{}
       (iterate stepper)
       (partition 2 1)
       (drop-while #(apply not= %))
       first
       first))

(defn parse
  "Treating `in` as a grid, return a set of [r c] locations of L's."
  [lines]
  (reduce (fn [acc [r l]]
            (into acc (keep-indexed (fn [c v] (when (= v \L) [r c])) l)))
          #{}
          (map-indexed vector lines)))

(def neighbors
  (memoize (fn neighbors [[r c]]
             (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
               [(+ r dr) (+ c dc)]))))

(defn puzzle1 [in]
  (let [seats (parse (str/split-lines in))]
    (count (stabilize (partial step seats neighbors 4)))))

(comment
  (let [input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"]
    (puzzle1 input))
  
  (let [input (slurp "input/2020/11-seating.txt")] (puzzle1 input)))

(defn on-grid? [rows cols [r c]]
  (and (<= 0 r (dec rows)) (<= 0 c (dec cols))))

(def visible-neighbors
  (memoize 
   (fn [seats rows cols [r c]]
     (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
       (->> (map (fn [[r c] i dr dc] [(+ r (* i dr)) (+ c (* i dc))])
                 (repeat [r c]) (drop 1 (range)) (repeat dr) (repeat dc))
            (filter #(or (seats %) (not (on-grid? rows cols %))))
            first)))))

(defn puzzle2 [in]
  (let [lines (str/split-lines in)
        [rows cols] ((juxt count #(count (first %))) lines)
        seats (parse lines)
        neighbors (partial visible-neighbors seats rows cols)]
    (count (stabilize (partial step seats neighbors 5)))))

(comment
  (let [input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"]
    (puzzle2 input))

  (let [input (slurp "input/2020/11-seating.txt")]
    (puzzle2 input)))
