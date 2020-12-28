(ns advent-of-code.2020.day-9
  (:require [clojure.string :as str]))

(defn sum-of-prev? [coll]
(let [targ (last coll)
      xs (butlast coll)
      xs' (map #(- targ %) xs)]
   (some (set xs') xs)))

(comment (sum-of-prev? '(35 20 15 25 47 40)))

(defn puzzle1 [x in]
  (->> (str/split-lines in)
       (map #(Long/parseLong %))
       (partition-all (inc x) 1)
       (drop-while sum-of-prev?)
       first
       last))

(defn find-range-sum-to [targ nums]
  (loop [i 0 j 0 sum 0]
    (cond
      (= sum targ) (map nums (range i j))
      (> sum targ) (recur (inc i) j (- sum (nums i)))
      (< sum targ) (recur i (inc j) (+ sum (nums j))))))

(defn puzzle2 [x in]
  (let [targ (puzzle1 x in)]
    (->> (find-range-sum-to targ (mapv #(Long/parseLong %) (str/split-lines in)))
         (apply (juxt min max))
         (reduce +))))

(comment
  (let [input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"]
    [(puzzle1 5 input)
     (puzzle2 5 input)])

  (puzzle1 25 (slurp "input/2020/9-xmas.txt")))

  (puzzle2 25 (slurp "input/2020/9-xmas.txt"))
