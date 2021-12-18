(ns advent-of-code.2021.day-18
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]))

;; --- Day 18: Snailfish ---
;; https://adventofcode.com/2021/day/18

(defn parse-input [s]
  (->> s str/split-lines (map edn/read-string)))

(defn regular-pair? [loc]
  (let [x (zip/node loc)]
    (and (vector? x) (number? (first x)) (number? (second x)))))

(defn explodable? [loc]
  (and (regular-pair? loc) (<= 4 (count (zip/path loc)))))

(defn splitable? [loc]
  (let [x (zip/node loc)] (and (number? x) (<= 10 x))))

(defn reg-neighbor
  "Find nearest neighbor of `loc` that is a regular number. Direction to search
   provided by `f` which should be one of `zip/prev` or `zip/next`. Return loc
   of neighbor or nil."
  [loc f]
  (loop [z (f loc)]
    (cond
      (or (nil? z) (zip/end? z)) nil
      (number? (zip/node z)) z
      :else (recur (f z)))))

(defn incr-neighbor
  "Increase a regular number neighbor of `loc` by amount `v`, searching in
   direction `dir` (must be `:next` or `:prev`). After the edit, find and return
   the loc of original node (by searching in the opposite direction). If no
   neighbor found, return original `loc`."
  [loc dir v]
  (let [[f f'] (if (= :next dir) [zip/next zip/prev] [zip/prev zip/next])]
    (if-let [z (reg-neighbor loc f)]
      (-> z (zip/edit + v) (reg-neighbor f'))
      loc)))

(defn explode
  "Returns sfn (vector) after exploding node at `loc`."
  [loc]
  (let [[r l] (zip/node loc)]
    (-> (zip/replace loc 0)
        (incr-neighbor :prev r)
        (incr-neighbor :next l)
        zip/root)))

(defn split
  "Return sfn after splitting node at `loc`."
  [loc]
  (let [v (zip/node loc)
        v' [(int (Math/floor (/ v 2))) (int (Math/ceil (/ v 2)))]]
    (-> loc (zip/replace v') zip/root)))

(defn reduce-with
  "Find first node in `sfn` that satisfies `pred` and edit it with `f`. Return
   a map with updated sfn and flag indicating if an edit occurred."
  [sfn pred f]
   (loop [z (zip/vector-zip sfn)]
     (cond
       (zip/end? z) {:edit? false :sfn (zip/root z)}
       (pred z) {:edit? true :sfn (f z)}
       :else (recur (zip/next z)))))

(defn reduce-sfn [sfn]
  (let [{:keys [edit? sfn]} (reduce-with sfn explodable? explode)]
    (if edit?
      (recur sfn)
      (let [{:keys [edit? sfn]} (reduce-with sfn splitable? split)]
        (if edit?
          (recur sfn)
          sfn)))))

(defn add-sfn [a b] (reduce-sfn [a b]))

(defn magnitude [sfn]
  (postwalk #(if (number? %) % (reduce + (map * % [3 2]))) sfn))

(comment
  (def example-1 "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]")

  (->> (parse-input example-1) (reduce add-sfn) magnitude)   ;=> 3488

  (def example-2 "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

  (->> (parse-input example-2) (reduce add-sfn) magnitude)   ;=> 4140

  (time
   (->> (slurp "input/2021/18-pairs.txt")
        parse-input (reduce add-sfn) magnitude))   ;=>4137

  (time
   (let [sfns (parse-input (slurp "input/2021/18-pairs.txt"))]
     (->> (for [a sfns b sfns :when (not= a b)] (add-sfn a b))
          (map magnitude)
          (reduce max))))
  )

(comment
  (->> [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
       reduce-sfn)

  (->> [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
       (zip/vector-zip)
       zip/next zip/next zip/next zip/next #_zip/next #_zip/next #_zip/next
       ((juxt regular-pair? zip/node zip/path #(count (zip/path %))))
       #_#_zip/next zip/next
       #_zip/path)
  )