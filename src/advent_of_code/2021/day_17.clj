(ns advent-of-code.2021.day-17)

;; --- Day 17: Trick Shot ---
;; https://adventofcode.com/2021/day/17

;; Input: target area: x=153..199, y=-114..-75

(def x-limits [153 199])
(def y-limits [-114 -75])

(defn z-on-target? [z limits] (let [[z-min z-max] limits] (<= z-min z z-max)))

(defn on-target? [[x y]]
  (and (z-on-target? x x-limits)
       (z-on-target? y y-limits)))

(defn still-in-range? [[x y]]
  (let [[_ x-max] x-limits [y-min _] y-limits]
    (and (<= x x-max) (>= y y-min))))

(defn regress [x] (cond (neg? x) (inc x) (pos? x) (dec x) :else x))

(defn trajectory [[x-vel y-vel]]
  (let [dcelr8 (fn [f] (fn [[pos vel]] [(+ pos vel) (f vel)]))
        points (fn [f x] (map first (iterate f [0 x])))]
    (map vector (points (dcelr8 regress) x-vel) (points (dcelr8 dec) y-vel))))

(comment
  (with-redefs [x-limits [20 30] y-limits [-10 -5]]
    (let [[_ x-max] x-limits [y-min _] y-limits]
      (->>
       (for [x-vel (range (inc x-max)) y-vel (range y-min (inc (- y-min)))]
         (->> (trajectory [x-vel y-vel])
              (take-while still-in-range?)
              (filter on-target?)))
       (keep seq)
       count)))   ;;=> 112

  ;; puzzle 1
  ;;    Um. I quickly solved this using a calculator and Excel in the few
  ;;    moments I had before heading up to the ski hill. Largest y-velocity
  ;;    allowed is 1 less than the minimum y-val: so 114-1=113, and the highest
  ;;    point it reaches is n*(n+1)/2=113*114/2=6441.

  ;; puzzle 2
  (let [[_ x-max] x-limits [y-min _] y-limits]
    (->>
     (for [x-vel (range (inc x-max)) y-vel (range y-min (inc (- y-min)))]
       (->> (trajectory [x-vel y-vel])
            (take-while still-in-range?)
            (filter on-target?)))
     (keep seq)
     count))   ;;=> 3186
  )