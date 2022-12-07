(ns advent-of-code.2022.day-07-directories
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as zip]))

(defn outer [loc] (if-not (zip/up loc) loc (recur (zip/up loc))))

(defn to-sibling [loc ch]
  (if (= ch (:dir (zip/node loc))) loc (recur (zip/right loc) ch)))

(defn make-tree [root]
  (zip/zipper :dir :children (fn [n c] (assoc n :children c)) root))

(defn build-tree [instr]
  (loop [z (make-tree {:dir "/"}) [[op1 op2 op3 :as cmd] & rest] instr]
    (if (not cmd)
      (zip/root z)
      (recur
       (cond
         (= 'cd op2) (cond
                       (= '/ op3) (outer z)
                       (= '.. op3) (zip/up z)
                       :else (to-sibling (zip/down z) (str op3)))
         (= 'dir op1) (zip/append-child z {:dir (str op2) :children []})
         (number? op1) (zip/append-child z {:file (str op2) :size op1})
         :else z)
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
       (map #(edn/read-string (str "[" % "]")))
       build-tree))

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/07-directories.txt"))
       find-sizes
       (filter #(< % 100000))
       (apply +))   ; => 1432936

  ;; puzzle 2
  (let [tree (parse (slurp "input/2022/07-directories.txt"))
        sizes (find-sizes tree)
        total-size (last sizes)
        unused (- 70000000 total-size)
        need (- 30000000 unused)]
    (->> sizes
         (filter #(>= % need))
         sort
         first))   ; => 272298
  )
