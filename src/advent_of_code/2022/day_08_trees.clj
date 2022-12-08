(ns advent-of-code.2022.day-08-trees
  (:require [advent-of-code.common :as common]))

(defn flank-at [n coll] [(reverse (take n coll)) (drop (inc n) coll)])

(defn take-upto
  "From https://github.com/weavejester/medley"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [x (first s)]
       (cons x (when-not (pred x) (take-upto pred (rest s))))))))

(defn survey [input]
  (let [{:keys [locmap] [wid hei] :size} (common/locmap<-digits input)
        height<- (fn [pos] (locmap pos))]
    (for [r (range wid) c (range hei)
          :let [[left right] (flank-at r (map #(height<- [% c]) (range wid)))
                [above below] (flank-at c (map #(height<- [r %]) (range hei)))
                h (get locmap [r c])]]
      {:pos [r c]
       :height h
       #_#_:search [left right above below]
       :visible? (->> [left right above below]
                      (map (fn [c] (every? #(> h %) c)))
                      (some true?))
       :scenic (->> [left right above below]
                    (map (fn [d] (take-upto #(<= h %) d)))
                    (map count)
                    (apply *))})))

(comment
  ;; puzzle 1
  (->> #_(survey "30373\n25512\n65332\n33549\n35390")
       (survey (slurp "input/2022/08-trees.txt"))
       (filter :visible?)
       count)   ; => 1719

  ;; puzzle 2
  (->> #_(survey "30373\n25512\n65332\n33549\n35390")
       (survey (slurp "input/2022/08-trees.txt"))
       (map :scenic)
       (apply max))   ; => 590824
  )