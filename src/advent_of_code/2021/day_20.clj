(ns advent-of-code.2021.day-20
  (:require [clojure.string :as str]))

;; Manage extrema.

(defn update-extrema [[[r-min r-max] [c-min c-max]] [r c]]
  [[(min r-min r) (max r-max r)] [(min c-min c) (max c-max c)]])

(defn expand-extrema [[[r-min r-max] [c-min c-max]]]
  [[(dec r-min) (inc r-max)] [(dec c-min) (inc c-max)]])

(defn points [[[r-min r-max] [c-min c-max]]]
  (for [r (range (dec r-min) (+ 2 r-max))
        c (range (dec c-min) (+ 2 c-max))]
    [r c]))

;; Parse input.

(defn parse-input [s]
  (let [pixel {\. 0, \# 1}
        [algo image] (str/split s #"\n\n")
        algo (mapv pixel algo)
        image (->> (str/split-lines image)
                   (map-indexed vector)
                   (mapcat (fn [[r l]]
                             (map-indexed (fn [c p] [[r c] (pixel p)]) l))))
        input (reduce (fn [acc [loc p]]
                        (-> acc
                            (update :image conj [loc p])
                            (update :extrema update-extrema loc)))
                      {:image {} :extrema [[##Inf ##-Inf] [##Inf ##-Inf]]}
                      image)]
    (assoc input :algo algo)))

(comment
  (let [{:keys [image algo extrema]}
        (parse-input (slurp "input/2021/20-image.txt"))]
    (println "image algo extrema" (count image) (count algo) extrema))
  )

;; Image enhancement.

(def bg-pixels (cycle [0 1]))   ; Toggle background between dark and light.

(defn neighbors [[r c]] (for [dr [-1 0 1] dc [-1 0 1]] [(+ r dr) (+ c dc)]))

(defn pixel [loc image algo bg]
  (let [nearby (for [ns (neighbors loc)] (get image ns bg))
        index (Integer/parseInt (apply str nearby) 2)]
  (get algo index)))

(defn enhance [algo bgs]
  (fn enhance
    ([{:keys [image extrema]}] (enhance image extrema bgs))
    ([image extrema [bg & bgs]]
     (let [output (into {}
                        (for [loc (points extrema)]
                          [loc (pixel loc image algo bg)]))]
       (lazy-seq
        (cons image (enhance output (expand-extrema extrema) bgs)))))))

(defn solve [n input]
  (->> input
       ((enhance (:algo input) bg-pixels))
       (drop n)
       first
       vals
       (reduce +)))

(comment
  (with-redefs [bg-pixels (repeat 0)]   ; No toggling on the sample puzzle.
    (solve 2 (parse-input (slurp "input/2021/20-image-test.txt"))))

  ;; part 1
  (solve 2 (parse-input (slurp "input/2021/20-image.txt")))

  ;; part 2
  (time
   (solve 50 (parse-input (slurp "input/2021/20-image.txt"))))
  )
