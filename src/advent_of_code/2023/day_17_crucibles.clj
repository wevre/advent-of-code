(ns advent-of-code.2023.day-17-crucibles
  (:require [advent-of-code.common2 :as common2]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [clojure.pprint :as pp]))

(def ^:dynamic *debug* false)

(def dir-chars {[1 0] \v, [-1 0] \^, [0 1] \>, [0 -1] \<})

(defn draw-path [locmap path]
  (let [[rows cols] (:size locmap)
        path-map (->> (partition 2 1 path)
                      (map (fn [[a b]] [a (dir-chars (mapv - a b))]))
                      (into {}))]
    (doseq [r (range rows)]
      (doseq [c (range cols)]
        (print (or (path-map [r c]) (locmap [r c]))))
      (println))
    (println)))

(def U [-1 0])
(def D [1 0])
(def R [0 1])
(def L [0 -1])

(def next-dirs
  {U [L R]
   D [L R]
   R [U D]
   L [U D]})

(def orient {U :V D :V R :H L :H})

(defn nigh's [locmap lo hi]
  (let [step-er (fn [∆]
                  (fn [{:keys [cost path loc]}]
                    (let [nxt (mapv + loc ∆)]
                      {:loc nxt :cost (+ cost (locmap nxt 0)) :dir ∆ :path (conj path nxt)})))]
    (fn [node]
      (for [∆ (get next-dirs (:dir node) [R D])
            nxt (->> (iterate (step-er ∆) node)
                     (take (inc hi))
                     (drop lo))
            :when (locmap (:loc nxt))]
        nxt))))

(defn update-costs [costs nxt]
  (update costs [(:loc nxt) (orient (:dir nxt))]
          (fnil (fn [cur] (if (< (:cost nxt) (:cost cur)) nxt cur)) nxt)))

(defn lowest-cost [locmap lo hi]
  (let [end (mapv dec (:size locmap))
        nigh's-er (nigh's locmap lo hi)
        loc [0 0] dir [1 1]
        node-key [loc :D]
        node {:loc loc :cost 0 :dir dir :path (list loc)}
        g-score (fn [{:keys [cost]}] cost)
        h-score (fn [{:keys [loc]}] (reduce + (map - end loc)))
        f-score (fn [n] (+ (g-score n) (h-score n)))]
    (loop [i 0 costs (priority-map-keyfn f-score node-key node)]
      (when-let [[[[loc _orient] node] costs] ((juxt peek pop) costs)]
        (if (= end loc)
          node
          (recur (inc i) (reduce update-costs costs (nigh's-er node))))))))

(comment
  (def locmap (into {} (common2/locmap<- #(- (int %) (int \0)) :?size true) (slurp "input/2023/17-sample.txt")))
  (def locmap (into {} (common2/locmap<- #(- (int %) (int \0)) :?size true) (slurp "input/2023/17-crucible.txt")))

  ;; year 2023 day 17 puzzle 1
  (time
   (:cost (lowest-cost locmap 1 3)))
  ;; => 1023 (56 seconds! so slow! don't care!)

  ;; year 2023 day 17 puzzle 2
  (time
   (:cost (lowest-cost locmap 4 10)))
  ;; => 1165 (42 seconds)

  )