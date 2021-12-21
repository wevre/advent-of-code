(ns advent-of-code.2021.day-20
  (:require [clojure.string :as str]))

(defn parse-input [s]
  (let [pixel {\. 0, \# 1}
        [algo image] (str/split s #"\n\n")
        algo (mapv pixel algo)
        image (->> (str/split-lines image)
                   (map-indexed vector)
                   (mapcat (fn [[r l]]
                             (map-indexed (fn [c p] [[r c] (pixel p)]) l))))
        input (reduce (fn [acc [[r c] p :as pix]]
                        (-> acc
                            (update :image conj pix)
                            (update :min-x min r)
                            (update :max-x max r)
                            (update :min-y min c)
                            (update :max-y max c)))
                      {:image {} :min-x ##Inf :max-x ##-Inf :min-y ##Inf :max-y ##-Inf}
                      image)]
    (assoc input :algo algo)))

(comment
  (let [{:keys [image algo min-x max-x min-y max-y]} (parse-input (slurp "input/2021/20-image.txt"))]
    (println "image algo min-x max-x min-y max-y" (count image) (count algo) min-x max-x min-y max-y))
  )

(def toggle-bg #(- 1 %))   ; Toggle infinite background between light and dark.

(defn neighbors [[r c]] (for [dr [-1 0 1] dc [-1 0 1]] [(+ r dr) (+ c dc)]))

(defn get-index [p image bg]
  (Integer/parseInt (apply str (for [ns (neighbors p)] (get image ns bg))) 2))

(defn enhance [algo]
 (fn [{:keys [image min-x max-x min-y max-y bg] :as input}]
   (let [output (for [r (range (dec min-x) (+ 2 max-x))
                      c (range (dec min-y) (+ 2 max-y))]
                  [[r c] (get algo (get-index [r c] image bg))])]
     (-> input
         (assoc :image (into {} output))
         (update :min-x dec)
         (update :max-x inc)
         (update :min-y dec)
         (update :max-y inc)
         (update :bg toggle-bg)))))

(defn solve [n input]
  (->> (assoc input :bg 0)
       (iterate (enhance (:algo input)))
       (drop n)
       first
       :image
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
