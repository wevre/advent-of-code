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

;; test this next's

(defn update-costs [costs nxt]
  (update costs (:loc nxt)
          (fnil (fn [cur] (if (< (:cost nxt) (:cost cur)) nxt cur)) nxt)))

(defn lowest-cost [locmap stop]
  (let [end (mapv dec (:size locmap))
        nigh's-er (nigh's locmap 1 3)
        loc [0 0]
        node {:loc loc :cost 0 :dir [1 1] :path (list loc)}
        g-score (fn [{:keys [cost]}] cost)
        h-score (fn [{:keys [loc]}] (reduce + (map - end loc)))
        f-score (fn [n] (+ (g-score n) (h-score n)))]
    (loop [i 0 costs (priority-map-keyfn f-score loc node)]
      (if (= i stop)
        costs
        (when-let [[[loc node] costs] ((juxt peek pop) costs)]
          (if (= end loc)
            node
            (let [nighs (nigh's-er node)
                  _ (when *debug* (println "curr node:") (pp/pprint node))
                  _ (when *debug* (println "nighs:") (pp/pprint nighs))
                  _ (when *debug* (println "costs:") (pp/pprint costs))]
              (recur (inc i) (reduce update-costs costs (nigh's-er node))))))))))

(comment
  (def locmap (into {} (common2/locmap<- #(- (int %) (int \0)) :?size true) (slurp "input/2023/17-sample.txt")))


  (let [info (lowest-cost locmap ##Inf)]
    (println "costs:")
    (pp/pprint info)
    (draw-path locmap (:path info)))

  (let [loc [0 0]]
    ()
    ((nigh's locmap 1 3) {:loc loc :cost 0 :dir [1 1] :path (loc)}))

  )