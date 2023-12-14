(ns advent-of-code.2023.day-13-mirrors
  (:require [advent-of-code.common :as common]))

(defn parse [input]
  (->> input
       common/split-grouped-lines
       (map (fn [lines] {:size [(count lines) (count (first lines))]
                         :rows lines
                         :cols (apply map vector lines)}))))

(defn compair   ;; <- LOL
  "Return the count of differences between the two series."
  [a b]
  (reduce + (map (fn [i j] (if (= i j) 0 1)) a b)))

(defn find-pairs
  "Examine adjacent line pairs and keep those with 0 <= differences <= 1."
  [info k]
  (assoc-in info [:pairs k]
            (->> (k info)
                 (map-indexed vector)
                 (partition 2 1)
                 (keep (fn [[[i a] [_ b]]]
                         (when (<= 0 (compair a b) 1) i))))))

(defn confirm-mirror
  "For a pair at index i, return i if the sum of differences of the origina
   pair and all the successive outward pairs is eps. For puzzle 1, eps will
   be 0, i.e. perfect match. For puzzle 2, eps will be 1, i.e. we will allow
   a single mismatch among all the com-pair-isons. (Ha! see what I did there?)"
  [i lines eps]
  (let [[bef aft] (split-at (inc i) lines)
        bef (reverse bef)]
    (when (= eps (reduce + (map (fn [a b] (compair a b)) bef aft))) i)))

(defn find-mirror [{:as info :keys [rows cols pairs]} eps]
  (let [confirm (fn [lines pairs] (or (first (keep #(confirm-mirror % lines eps) pairs)) -1))]
    (-> info
        (assoc-in [:mirror :rows] (confirm rows (:rows pairs)))
        (assoc-in [:mirror :cols] (confirm cols (:cols pairs))))))

(defn solve [eps]
  (fn [info]
    (-> info
        (find-pairs :rows)
        (find-pairs :cols)
        (find-mirror eps))))

(defn score [{:as info {:keys [rows cols]} :mirror}]
  (assoc info :score (+ (inc cols) (* 100 (inc rows)))))

(comment
  (def input (parse (slurp "input/2023/13-sample.txt")))
  (def input (parse (slurp "input/2023/13-mirrors.txt")))

  ;; year 2023 day 13 puzzle 1
  (->> input
       (map (solve 0))
       (map score)
       (map :score)
       (reduce +))
  ;; => 34202

  ;; year 2023 day 13 puzzle 1
  (->> input
       (map (solve 1))
       (map score)
       (map :score)
       (reduce +))
  ;; => 34230
  )
