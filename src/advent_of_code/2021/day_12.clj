(ns advent-of-code.2021.day-12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 12: Passage Pathing ---
;; https://adventofcode.com/2021/day/12

(defn parse-input
  "Returns map of connections: key is cave, value is set of connected caves."
  [s]
  (->> (str/split-lines s)
       (map #(re-seq #"\w+" %))
       (mapcat (fn [[cave1 cave2]] [{cave1 #{cave2}} {cave2 #{cave1}}]))
       (apply merge-with set/union)))

(def start "start")
(def end "end")

(defn small-cave? [cave] (= cave (str/lower-case cave)))

(defn find-paths
  [cave-map limit]
  (fn find-paths [path]
    (let [cave (peek path)
          small-caves (filter small-cave? path)
          visit-counts (-> (frequencies small-caves) (dissoc start) vals)
          visited (if (some #{limit} visit-counts) (set small-caves) #{start})
          allowed (remove visited (cave-map cave))]
      (cond
        (= end cave) [path]
        :else (mapcat #(find-paths (conj path %)) allowed)))))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (let [cave-map (parse-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")
        finder (find-paths cave-map 2)]
    (->> (finder [start]) (map #(str/join "," %)) sort pprint)))

(comment
  ;; puzzle 1
  (let [cave-map (parse-input (slurp "input/2021/12-caves.txt"))
        finder (find-paths cave-map 1)]
    (count (finder [start])))

  ;; puzzle 2
  (let [cave-map (parse-input (slurp "input/2021/12-caves.txt"))
        finder (find-paths cave-map 2)]
    (count (finder [start])))
  )
