(ns advent-of-code.2023.day-10-v2
  (:require [advent-of-code.common :as common]
            [advent-of-code.common2 :as common2]
            [clojure.string :as str]))

(def next<- {\- {[0  1] [0  1]
                 [0 -1] [0 -1]}
             \| {[1  0] [1  0]
                 [-1  0] [-1  0]}
             \F {[-1  0] [0  1]
                 [0 -1] [1  0]}
             \J {[1  0] [0 -1]
                 [0  1] [-1  0]}
             \L {[1  0] [0  1]
                 [0 -1] [-1  0]}
             \7 {[0  1] [1  0]
                 [-1  0] [0 -1]}})

(defn find-path [lmap]
  (fn [loc from path]
    (let [p (lmap loc)]
      (if (= \S p)
        (conj path loc)
        (let [to (get-in next<- [p from])]
          (recur (mapv + loc to) to (conj path loc)))))))

(defn ?inside [[r c :as loc] path lmap]
  (when-not (path loc)
    (let [ray (str/join (for [i (range r)
                              :let [loc [i c]]
                              :when (path loc)
                              :let [p (lmap loc)]
                              :when (not= p \|)]
                          p))
          crossings (re-seq #"FJ|7L|-" ray)]
      (odd? (count crossings)))))

(comment
  (def input (slurp "input/2023/10-pipes.txt"))

  (def locmap (into {} (common2/locmap<-) input))

  (def start (->> locmap (keep (fn [[k v]] (when (= v \S) k))) first))
  ;; [92 43]

  ;; The map around S is
  ;; L--
  ;; FS-
  ;; |F-
  ;; so S is really -

  (def path ((find-path locmap) [92 42] [0 -1] #{}))

  ;; year 2023 day 10 puzzle 1
  (/ (count path) 2)
  ;; => 7107

  ;; year 2023 day 10 puzzle 2
  (->> locmap
       (filter #(?inside (key %) path (assoc locmap start \-)))
       count)
  ;; => 281
  )
