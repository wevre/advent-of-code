(ns advent-of-code.2023.day-17-v2
  (:require [advent-of-code.common2 :as common2]
            [advent-of-code.common.astar :as astar]))

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

(defrecord Node [loc dir cost nigh's-er end]
  astar/INode
  (node-key [_] [loc (orient dir)])
  (g-score [_] cost)
  (h-score [_] (let [[r c] loc [er ec] end] (+ (- er r) (- ec c))))
  (nigh's [this] (nigh's-er this))
  (?end [_] (= loc end)))

(defn nigh's [locmap lo hi node<-]
  (let [step-er (fn [∆]
                  (fn [{:keys [cost loc]}]
                    (let [nxt (mapv + loc ∆)]
                      (node<- :loc nxt :dir ∆ :cost (+ cost (locmap nxt 0))))))]
    (fn [node]
      (for [∆ (get next-dirs (:dir node) [R D])
            nxt (->> (iterate (step-er ∆) node)
                     (take (inc hi))
                     (drop lo))
            :when (locmap (:loc nxt))]
        nxt))))

(defn solve [locmap lo hi]
  (let [end (mapv dec (:size locmap))
        node<- (fn node<- [& {:keys [loc dir cost]}]
                 (->Node loc dir cost (nigh's locmap lo hi node<-) end))]
    (astar/lowest-cost (node<- :loc [0 0] :dir nil :cost 0))))

(comment
  (def locmap (into {} (common2/locmap<- #(- (int %) (int \0)) :?size true) (slurp "input/2023/17-sample.txt")))
  (def locmap (into {} (common2/locmap<- #(- (int %) (int \0)) :?size true) (slurp "input/2023/17-crucible.txt")))

  ;; year 2023 day 17 puzzle 1
  (time
   (:cost (solve locmap 1 3)))
  ;; => 1023 (56 seconds! so slow! don't care!)

  ;; year 2023 day 17 puzzle 2
  (time
   (:cost (lowest-cost locmap 4 10)))
  ;; => 1165 (42 seconds)
  )