(ns advent-of-code.2021.day-19
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(defn parse-scanner [s]
  (let [[name & bs] (str/split-lines s)
        i (->> (re-seq #"\d+" name) first parse-long)]
    {:scanner i
     :beacons (->> (mapcat #(re-seq #"-?\d+" %) bs)
                   (map parse-long)
                   (partition 3)
                   set)}))

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map parse-scanner)))

(defn all-orientations [[x y z]]
  (concat
   (for [[x y z] [[x y z] [y z x] [z x y]]
         [sx sy sz] [[+ + +] [+ - -] [- + -] [- - +]]]
     [(sx x) (sy y) (sz z)])
   (for [[x y z] [[x z y] [z y x] [y x z]]
         [sx sy sz] [[+ - +] [+ + -] [- + +] [- - -]]]
     [(sx x) (sy y) (sz z)])))

(defn offset
  "For two sets of beacons, and measuring all deltas between their points, if we
   find a common delta for more than 12 points, we've got a match."
  [bs1 bs2]
  (->> (for [b1 bs1 b2 bs2] (map - b1 b2))
       (group-by identity)
       vals
       (some #(when (<= 12 (count %)) (first %)))))

(defn try-offsets
  "Check for offset between s1 and all orientations of s2. If we find one,
   return that offset and transformed beacons of s2."
  [s1 s2]
  (some
   #(when-let [offset (offset (:beacons s1) %)]
      {:scanner (:scanner s2)
       :offset offset
       :beacons (map (fn [x] (map + offset x)) %)})
   (->> (map all-orientations (:beacons s2)) (apply map vector))))

(defn solve [[s0 & ss]]
  (loop [base s0
         offsets [{:scanner 0 :offset [0 0 0]}]
         [curr & rest] ss
         fail ()]   ; Place to keep un-found scanners.
    (if curr
      (if-let [found (try-offsets base curr)]
        (recur (update base :beacons into (:beacons found))
               (conj offsets (select-keys found [:scanner :offset]))
               (into rest fail)   ; Reset all un-found scanners.
               ())
        (recur base offsets rest (conj fail curr)))
      {:beacons (:beacons base) :offsets offsets :fail fail})))

(comment
  ;; part 1
  (time
   (->> (solve (parse-input (slurp "input/2021/19-scanners.txt")))
        :beacons
        count))

  ;; part 2
  (time
   (let [locs (->> (solve (parse-input (slurp "input/2021/19-scanners.txt")))
                   :offsets
                   (map :offset))]
     (->> (for [a locs b locs :when (not= a b)]
            (->> (map - a b) (map math/abs) (reduce +)))
          (apply max))))
  )
