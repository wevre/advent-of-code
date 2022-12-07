(ns advent-of-code.2022.day-07-directories
  (:require [clojure.core.match :as m]
            [clojure.string :as str]
            [clojure.zip :as zip]))

(defn zip-outer [loc] (if-not (zip/up loc) loc (recur (zip/up loc))))

(defn zip-sibling [loc ch]
  (if (= ch (:dir (zip/node loc))) loc (recur (zip/right loc) ch)))

(defn build-tree [instr]
  (loop [z (zip/zipper :dir :children #(assoc %1 :children %2) {:dir "/"})
         [cmd & rest] instr]
    (if-not cmd
      (zip/root z)
      (recur
       (m/match cmd
         ["$" "ls"] z
         ["$" "cd" "/"] (zip-outer z)
         ["$" "cd" ".."] (zip/up z)
         ["$" "cd" dir] (zip-sibling (zip/down z) dir)
         ["dir" dir] (zip/append-child z {:dir dir :children []})
         [size name] (zip/append-child z {:file name :size (parse-long size)}))
       rest))))

(defn find-sizes
  ([root] (-> (find-sizes [[] 0] root) first))
  ([[out sum] node]
   (if (:dir node)
     (let [[ro rs] (reduce find-sizes [out 0] (:children node))]
       [(conj ro rs) (+ sum rs)])
     [out (+ sum (:size node))])))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       build-tree))

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/07-directories.txt"))
       find-sizes
       (filter #(< % 100000))
       (apply +))   ; => 1432936

  ;; puzzle 2
  (let [sizes (find-sizes (parse (slurp "input/2022/07-directories.txt")))
        total-size (last sizes)
        unused (- 70000000 total-size)
        need (- 30000000 unused)]
    (->> sizes
         (filter #(>= % need))
         sort
         first))   ; => 272298
  )
