(ns advent-of-code.2022.day-07-directories
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.walk :as walk]))

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

(defn add-dir-size [root]
  (walk/postwalk (fn [x]
                   (if (and (map? x) (:dir x))
                     (assoc x :size (->> x :children (map :size) (apply +)))
                     x))
                 root))

(defn find-dir-sizes [root]
  (loop [z (make-tree root) dirs []]
    (if (zip/end? z)
      dirs
      (let [{:keys [dir size]} (zip/node z)]
        (recur (zip/next z) (cond-> dirs dir (conj size)))))))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(edn/read-string (str "[" % "]")))
       build-tree
       add-dir-size))

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/07-directories.txt"))
       find-dir-sizes
       (filter #(< % 100000))
       (apply +))   ; => 1432936

  ;; puzzle 2
  (let [tree (parse (slurp "input/2022/07-directories.txt"))
        total-size (:size tree)
        unused (- 70000000 total-size)
        need (- 30000000 unused)]
    (->> tree
         find-dir-sizes
         (filter #(>= % need))
         sort
         first))

  ;; sample puzzle
  (let [instr '([$ cd /]
                [$ ls]
                [dir a]
                [14848514 b.txt]
                [8504156 c.dat]
                [dir d]
                [$ cd a]
                [$ ls]
                [dir e]
                [29116 f]
                [2557 g]
                [62596 h.lst]
                [$ cd e]
                [$ ls]
                [584 i]
                [$ cd ..]
                [$ cd ..]
                [$ cd d]
                [$ ls]
                [4060174 j]
                [8033020 d.log]
                [5626152 d.ext]
                [7214296 k])]
    (-> (build-tree instr)
        add-dir-size
        find-dirs))
  )