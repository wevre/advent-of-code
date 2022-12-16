(ns advent-of-code.2022.day-15-beacons
  (:require [advent-of-code.common :as common]
            [clojure.java.math :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

;; 2022-12-15 00:24
;;    This code isn't very inspiring. I'm too tired to be more clever here. My
;;    first attempt at Part 2 took over 10 minutes to run. Then I tried pmap and
;;    got it down to about 2.5 minutes. Still seems slow.
;; 2022-12-15 00:37
;;    I have an idea of a different way to do this, but I'll have to attempt it
;;    after some sleep.
;; 2022-12-15 15:28
;;    Did five runs down the ski hill, got lunch, attended a meeting at work and
;;    made sure my emails were caught up, then did a new approach for Part 2.
;;    Clocking in at 45 msecs, a 3000x improvement!
;; 2022-12-15 18:27
;;    Did some cleanup and some extensive commenting. Also using combinatorics I
;;    got the runtime for Part 2 down to 11 msecs. Whoa!

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
  "Return map of sensor-beacon data, including outer rectangle (a diamond shape
   specified by min and max x-naughts and y-naughts) and manhattan distance.
   Storing rectangles using naughts makes them easy to compare and find
   intersections.

                o
              ╱   ╲
          xa╱       ╲yb
          ╱           ╲
        o               o
          ╲           ╱
          ya╲       ╱xb
              ╲   ╱
                o
   "
  [[sx sy :as sensor] [bx by :as beacon]]
  (let [d (m-dist [sx sy] [bx by])
        xa (xnaught [(- sx d) sy]) ya (ynaught [sx (- sy d)])
        xb (xnaught [(+ sx d) sy]) yb (ynaught [sx (+ sy d)])]
    {:rect {:min [xa ya] :max [xb yb]} :dist d
     :sensor sensor :beacon beacon}))

(defn vesica
  "Return rectangular region formed by intersection of two rects or nil.
   Conceptually similar to vesica pisces formed by intersection of two circles.

                              o
                  o       xc╱   ╲
                ╱   ╲yb   ╱       ╲
              ╱       ╲W╱           ╲yd
          xa╱       xe╱ ╲             ╲
          ╱         o     ╲yf           o
        o           ye╲     o         ╱
          ╲             ╲ ╱xf       ╱xd
          ya╲           ╱W╲       ╱
              ╲       ╱xb   ╲   ╱
                ╲   ╱         o
                  o

   Above, xe, xf and ye, yf are the naughts of intersection region, the vesica.
   Rectangles that don't overlap produce naughts that are out of order and will
   cause this function to return nil.

   The W's in the diagram above are points we are trying to find as explained in
   the `outside-elbows` function below.
   "
  [{:as _rect1 [xa ya] :min [xb yb] :max}
   {:as _rect2 [xc yc] :min [xd yd] :max}]
  (let [xe (max xa xc) ye (max ya yc)
        xf (min xb xd) yf (min yb yd)]
    (when (and (<= xe xf) (<= ye yf))
      {:min [xe ye] :max [xf yf]})))

(defn elbows<-rect
  "Return four points _just outside_ the corners of rect, W's in the diagram
   below. The top elbow, for example, is intersection of xe and yf plus [0 +1].

         W
         o
     xe╱   ╲yf
   W o       ╲
       ╲       o W
       ye╲   ╱xf
           o
           W
   "
  [{[xe ye] :min [xf yf] :max}]
  (map (fn [x0 y0 ∆] (map + (pnt<-naughts x0 y0) ∆))
       [xe xe xf xf]   ; order (for all three) is top, left, right, bottom
       [yf ye yf ye]
       [[0 +1] [-1 0] [+1 0] [0 -1] []]))

(defn outside?
  "True if point `p` is outside rect using manhattan distance."
  [p {:keys [sensor dist]}]
  (> (m-dist p sensor) dist))

(defn outside-elbows
  "Find intersection (vesica) of two rects, and return the elbows of that vesica
   that are outside both rects."
  [{r1 :rect d1 :dist s1 :sensor} {r2 :rect d2 :dist s2 :sensor}]
  (when-let [v (vesica r1 r2)]
    (->> v
         elbows<-rect
         (map (juxt identity #(m-dist % s1) #(m-dist % s2)))
         (keep (fn [[p da db]] (when (= (- d1 da) (- d2 db)) p))))))

(defn find-outside-beacons
  "Find all the elbows between all the pairwise combinations of rects and keep
   only those elbows that are not inside any rects."
  [infos]
  (->> (combo/combinations infos 2)
       (keep #(apply outside-elbows %))
       (apply concat)
       distinct
       (filter (fn [elb] (every? #(outside? elb %) infos)))))

(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)
       (map #(partition 2 %))
       (map #(apply info<-sb %))))

(comment
  ;; puzzle 2 -- 11 msecs
  (time
   (let [limit 4000000 #_20
         infos (parse (slurp "input/2022/15-beacons.txt"))]
     (->> infos
          find-outside-beacons
          (filter (fn [[x y]] (and (<= 0 x limit) (<= 0 y limit))))
          (map (fn [[x y]] (+ y (* 4000000 x))))
          first)))
  )

#_(defn parse [input]
  (->> input
       str/split-lines
       (map common/parse-longs)))

(defn overlap? [a b c d]
  (and (<= c (inc b)) (>= d (dec a))))

(defn combine [a b c d]
  (when (overlap? a b c d)
    [(min a c) (max b d)]))

(defn no-beacons [[sx sy bx by] y-targ]
  (let [mdist (+ (math/abs (- bx sx)) (math/abs (- by sy)))
        vdist (math/abs (- y-targ sy))
        hwid (- mdist vdist)]
    (when (pos? hwid) [(- sx hwid) (+ sx hwid)])))

(defn merge-spans [spans]
  (reduce (fn [acc [minx maxx]]
            (loop [[[a b :as n] & r] acc out [] minx minx maxx maxx]
              (if-not n
                (conj out [minx maxx])
                (if-let [[minx maxx] (combine a b minx maxx)]
                  (recur r out minx maxx)
                  (recur r (conj out n) minx maxx)))))
          []
          spans))

(def ^:dynamic *targ-row* 2000000)

(def ^:dynamic *input* "input/2022/15-beacons.txt")

(def ^:dynamic *max-coord* 4000000)

(defn find-gaps [parsed max-x]
  (fn [y]
    (let [spans (->> parsed (keep #(no-beacons % y)) merge-spans)]
      (when (some (fn [[lo hi]] (not (<= lo 0 max-x hi))) spans)
        [y spans]))))

(comment
  ;; puzzle 1
  (binding [*targ-row* *targ-row* #_10
            *input* *input* #_"input/2022/15-beacons-sample.txt"]
    (let [parsed (parse (slurp *input*))
          spans (->> parsed
                     (keep (fn [data] (no-beacons data *targ-row*)))
                     merge-spans)
          beacons (->> parsed
                       (keep (fn [[_ _ bx by]]
                               (when (some (fn [[lo hi]]
                                             (and (= by *targ-row*)
                                                  (<= lo bx hi)))
                                           spans)
                                 [bx by])))
                       distinct)
          lengths (->> spans (map (fn [[a b]] (+ 1 (- b a)))) (apply +))]
      (- lengths (count beacons))))   ; => 6124805
  )
