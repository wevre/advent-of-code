(ns advent-of-code.2022.day-08-trees
  (:require [advent-of-code.common :as common]))

(defn skip-at [n coll] [(reverse (take n coll)) (drop (inc n) coll)])

(defn unblocked [h coll]
  (let [[a b] (split-with #(< % h) coll)]
    (+ (count a) (count (take 1 b)))))

(defn survey [input]
  (let [{:keys [locmap] [w h] :size} (common/locmap<-digits input)
        height (fn [pos] (locmap pos))]
    (for [r (range w) c (range h)
          :let [[left right] (skip-at r (map #(height [% c]) (range w)))
                [above below] (skip-at c (map #(height [r %]) (range h)))
                hei (get locmap [r c])]]
      {:pos [r c] :height hei #_#_:search [left right above below]
       :visible? (or (every? #(> hei %) left)
                     (every? #(> hei %) right)
                     (every? #(> hei %) above)
                     (every? #(> hei %) below))
       :scenic (->> [left right above below]
                    (map #(unblocked hei %))
                    (apply *))})))

(comment
  ;; puzzle 1
  (->> (survey (slurp "input/2022/08-trees.txt"))
       (filter :visible?)
       count)   ; => 1719

  ;; puzzle 2
  (->> (survey (slurp "input/2022/08-trees.txt"))
       (map :scenic)
       (apply max))   ; => 590824
  )