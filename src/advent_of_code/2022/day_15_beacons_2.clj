(ns advent-of-code.2022.day-15-beacons-2
  (:require [advent-of-code.common :as common]
            [clojure.java.math :as math]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; 2022-12-20
;;    This is a simpler version of original code. Instead of finding the
;;    overlapping region (the vesica) of the two rectangles, we just find all
;;    the intersections of all the edges and keep only those that are _just_
;;    outside.

(defn xnaught
  "x-intercept of line with slope +1 passing through [x y]."
  [[x y]]
  (- x y))

(defn ynaught
  "y-intercept of line with slope -1 passing through [x y]."
  [[x y]]
  (+ x y))

(defn pnt<-naughts
  "Point at intersection of x-naught, y-naught."
  [x0 y0]
  [(/ (+ x0 y0) 2) (/ (- y0 x0) 2)])

(defn m-dist
  "Manhattan distance between two points."
  [p1 p2]
  (->> (map - p1 p2) (map long) (map math/abs) (apply +)))

(defn info<-sb
  [[sx sy :as sensor] [bx by :as beacon]]
  (let [d (m-dist [sx sy] [bx by])
        xa (xnaught [(- sx d) sy]) ya (ynaught [sx (- sy d)])
        xb (xnaught [(+ sx d) sy]) yb (ynaught [sx (+ sy d)])]
    {:rect {:min [xa ya] :max [xb yb]}
     :dist d
     :sensor sensor :beacon beacon}))

(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)
       (map #(partition 2 %))
       (map #(apply info<-sb %))))

(defn find-elbows [{{[xa ya] :min [xb yb] :max} :rect d1 :dist s1 :sensor}
                   {{[xc yc] :min [xd yd] :max} :rect d2 :dist s2 :sensor}]
  (let [pnts (fn [xs ys]
               (for [x xs y ys
                     :let [pnt (pnt<-naughts x y)]
                     :when (and (= (m-dist pnt s1) (inc d1)) (= (m-dist pnt s2) (inc d2)))]
                 pnt))]
    (concat (pnts [(dec xa) (inc xb)] [(dec yc) (inc yd)])
            (pnts [(dec xc) (inc xd)] [(dec ya) (inc yb)]))))

(defn reflect [y {[sx sy] :sensor [bx by] :beacon}]
  (info<-sb [sx (- (* 2 y) sy)] [bx (- (* 2 y) by)]))

(defn outside?
  [p {:keys [sensor dist]}]
  (> (m-dist p sensor) dist))

(comment
  ;; puzzle 1
  (let [infos (parse (slurp "input/2022/15-beacons.txt"))
        targ-row 2000000
        [elb1 elb2] (->> infos
                         (mapcat #(find-elbows % (reflect targ-row %)))
                         (map first)
                         (apply (juxt min max)))
        beacons-on-target (->> infos
                               (map :beacon)
                               (keep (fn [[x y]] (when (= targ-row y) x)))
                               distinct
                               count)]
    (- (- elb2 elb1 1) beacons-on-target))   ; => 6124805

  ;; puzzle 2
  (time
   (let [limit 4000000 #_20
         infos (parse (slurp "input/2022/15-beacons.txt"))]
     (->> (combo/combinations infos 2)
          (mapcat #(apply find-elbows %))
          distinct
          (filter (fn [elb] (every? #(outside? elb %) infos)))
          (filter (fn [[x y]] (and (<= 0 x limit) (<= 0 y limit))))
          (map (fn [[x y]] (+ y (* 4000000 x))))
          first)))   ; => 12555527364986
  )