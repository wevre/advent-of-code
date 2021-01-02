(ns advent-of-code.2020.day-24
  (:require [clojure.string :as str]
           [clojure.edn :as edn]))

;; --- Day 24: Lobby Layout ---

(def delta {"e" [1 0] "w" [-1 0] "nw" [0 1] "ne" [1 1] "se" [0 -1] "sw" [-1 -1]})

(defn get-pattern [input]
  (->> (str/split-lines input)
       (map #(re-seq #"e|w|nw|ne|sw|se" %))
       (map #(reduce (fn [loc cmd] (mapv + loc (delta cmd))) [0 0] %))
       frequencies
       (keep #(when (odd? (val %)) (key %)))))

(defn puzzle1 [input]
  (count (get-pattern input)))

(defn neighbors [tile]
  (map #(mapv + tile %) (vals delta)))

(defn step [tiles]
  (set (for [[loc n] (frequencies (mapcat neighbors tiles))
             :when (or (= 2 n) (and (tiles loc) (= 1 n)))]
         loc)))

(defn puzzle2 [n input]
  (->> (set (get-pattern input))
       (iterate step)
       (drop n)
       first
       count))

(comment
  
  (puzzle2 100 (slurp "input/2020/24-tiles.txt"))
  
  (puzzle2 100 "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"))
