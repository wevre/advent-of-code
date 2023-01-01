(ns advent-of-code.2022.day-23-elf-diffusion
  (:require [advent-of-code.common :as common]
            [clojure.math.combinatorics :as combo]))

(defn- add [& locs] (apply mapv + locs))

(defn- nighs
  "The neighbors of a particular `loc`."
  [loc]
  (->> (combo/selections [-1 0 1] (count loc))
       (remove #(apply = 0 %))
       (map #(add loc %))))

(defn- validator
  "Returns a function that takes a triplet of neighbors and, if they are all
   empty, returns a tuple of new loc (the middle of the neighbor triplet) and
   original loc, or nil."
  [loc elfs]
  (fn [[_a b _c :as ∆s]]
    (when (not-any? elfs (map #(add loc %) ∆s))
      [(add loc b) loc])))

(defn- proposed
  "Applies rules in order and returns first valid tuple of [whither whence]."
  [loc elfs tests]
  (let [valid? (validator loc elfs)]
    (when (some elfs (nighs loc))
      (some valid? tests))))

(def tests
  "Returns cycle of tests. First element is "
  (let [indices (cycle [-1 -1 -1 +1 +1 +1 -1 0 +1 -1 0 +1])]
    (->>
     (map vector indices (drop 6 indices))
     (partition 3)
     (partition 4 1))))

(defn step
  "Finds proposed moves, filters those hivng only one hopeful, then applies."
  [elfs tests]
  (->> elfs
       (keep #(proposed % elfs tests))
       (group-by first)
       (keep #(when (= 1 (count (val %))) (first (val %))))
       (reduce (fn [elfs [whence whither]] (-> elfs (conj whence) (disj whither))) elfs)))

(defn parse [input]
  (->> input
       common/locmap<-
       :locmap
       (reduce-kv (fn [m k v] (cond-> m (= v \#) (conj k))) #{})))

(defn bounds [elfs]
  (reduce (fn [[emin emax] elf]
            [(mapv min emin elf) (mapv max emax elf)])
          [[##Inf ##Inf] [##-Inf ##-Inf]]
          elfs))

(defn draw [elfs]
  (let [[[minr minc] [maxr maxc]] (bounds elfs)]
    (->> (for [r (range minr (inc maxr))]
           (concat (for [c (range minc (inc maxc))]
                     (if (elfs [r c]) \# \.))
                   [\newline]))
         flatten
         (apply str))))

(comment
  ;; puzzle 1
  (let [elfs (parse (slurp "input/2022/23-elf-diffusion.txt"))
        outcome (->> tests (reductions step elfs) (drop 10) first)
        [[minr minc] [maxr maxc]] (bounds outcome)]
    (- (* (- maxr minr -1) (- maxc minc -1)) (count elfs)))   ; => 4195

  ;; puzzle 2 -- 42s
  (time
   (let [elfs (parse (slurp "input/2022/23-elf-diffusion.txt"))]
     (->> tests
          (reductions step elfs)
          (partition 2 1)
          (take-while #(apply not= %))
          count
          inc)))   ; => 1069
  )
