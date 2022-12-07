(ns advent-of-code.2022.day-07-directories
  (:require [clojure.core.match :as m]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]))

(defn build-tree [instr]
  (loop [z (zip/zipper :dir :children #(assoc %1 :children %2) {:dir "/"})
         [cmd & rest] instr]
    (if-not cmd
      (zip/root z)
      (recur
       (m/match cmd
         ["$" "ls"] z
         ["$" "cd" "/"] (->> z (iterate zip/up) (take-while identity) last)
         ["$" "cd" ".."] (zip/up z)
         ["$" "cd" dir] (->> (zip/down z)
                             (iterate zip/right)
                             (filter #(= dir (:dir (zip/node %))))
                             first)
         ["dir" dir] (zip/append-child z {:dir dir :children []})
         [size name] (zip/append-child z {:file name :size (parse-long size)}))
       rest))))

(defn find-sizes [root]
  (->> root
       (walk/postwalk (fn [x] (cond
                                (and (map? x) (:dir x)) (:children x)
                                (and (map? x) (:file x)) (:size x)
                                :else x)))
       (tree-seq seq? identity)
       (keep #(when (seq? %) (apply + (flatten %))))))

(defn parse [input]
  (->> input str/split-lines (map #(str/split % #" ")) build-tree find-sizes))

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/07-directories.txt"))
       (filter #(< % 100000))
       (apply +))   ; => 1432936

  ;; puzzle 2
  (let [sizes (parse (slurp "input/2022/07-directories.txt"))
        total-size (apply max sizes)
        unused (- 70000000 total-size)
        need (- 30000000 unused)]
    (->> sizes
         (filter #(>= % need))
         sort
         first))   ; => 272298
  )
