(ns advent-of-code.2021.day-20
  (:require [clojure.string :as str]))

(defn update-extrema [[x-min x-max] x] [(min x-min x) (max x-max x)])

(defn parse-input [s]
  (let [pixel {\. 0, \# 1}
        [algo image] (str/split s #"\n\n")
        algo (mapv pixel algo)
        image (->> (str/split-lines image)
                   (map-indexed vector)
                   (mapcat (fn [[r l]]
                             (map-indexed (fn [c p] [[r c] (pixel p)]) l))))
        input (reduce (fn [acc [[r c] _p :as pix]]
                        (-> acc
                            (update :image conj pix)
                            (update :r-extrema update-extrema r)
                            (update :c-extrema update-extrema c)))
                      {:image {} :r-extrema [##Inf ##-Inf] :c-extrema [##Inf ##-Inf]}
                      image)]
    (assoc input :algo algo)))

(comment
  (let [{:keys [image algo r-extrema c-extrema]} (parse-input (slurp "input/2021/20-image.txt"))]
    (println "image algo r-extrema c-extrema" (count image) (count algo) r-extrema c-extrema))
  )

(def toggle-bg #(- 1 %))   ; Toggle infinite background between light and dark.

(defn neighbors [[r c]] (for [dr [-1 0 1] dc [-1 0 1]] [(+ r dr) (+ c dc)]))

(defn get-index [p image bg]
  (Integer/parseInt (apply str (for [ns (neighbors p)] (get image ns bg))) 2))

(defn expand-extrema [[x-min x-max]] [(dec x-min) (inc x-max)])

(defn range<-extrema [[x-min x-max]] (range (dec x-min) (+ 2 x-max)))

(defn enhance [algo]
  (fn enhance
    ([{:keys [image r-extrema c-extrema bg]}] (enhance image r-extrema c-extrema bg))
    ([image r-extrema c-extrema bg]
     (let [output (into {} (for [r (range<-extrema r-extrema)
                                 c (range<-extrema c-extrema)]
                             [[r c] (get algo (get-index [r c] image bg))]))]
       (lazy-seq (cons image (enhance output
                                      (expand-extrema r-extrema)
                                      (expand-extrema c-extrema)
                                      (toggle-bg bg))))))))

(defn solve [n input]
  (->> (assoc input :bg 0)
       ((enhance (:algo input)))
       (drop n)
       first
       vals
       (reduce +)))

(comment
  (with-redefs [toggle-bg identity]   ; No toggling on the sample puzzle.
    (solve 2 (parse-input (slurp "input/2021/20-image-test.txt"))))

  ;; part 1
  (solve 2 (parse-input (slurp "input/2021/20-image.txt")))

  ;; part 2
  (time
   (solve 50 (parse-input (slurp "input/2021/20-image.txt"))))
  )
