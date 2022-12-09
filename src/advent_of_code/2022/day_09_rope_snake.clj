(ns advent-of-code.2022.day-09-rope-snake
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ∆s {'U [+1 0] 'L [0 -1] 'D [-1 0] 'R [0 +1]})

(defn signum [x] (cond (zero? x) 0 (pos? x) +1 :else -1))

(defn pull
  "Given old and new positions of first knot, and current position of second
   knot, return new position of second knot."
  [[hr hc] [tr tc :as k2]]
  (let [∆r (- hr tr) ∆c (- hc tc)]
    (cond
      (and (<= -1 ∆r +1) (<= -1 ∆c +1)) k2
      :else [(+ tr (signum ∆r)) (+ tc (signum ∆c))])))

(comment
  (pull [1 4] [0 2])   ; => [1 3]
  (Math/signum 3)
  )

;; TODO: instead of tracking a big dictionary, we could just track a vector of
;; pos. The first one is head and is updated by the next instruction, the
;; remainder are updated following that. That way we can start with a rope of
;; any size (2 knots, 10 knots, whatever)

(def ks '(H 1 2 3 4 5 6 7 8 9))

(defn move [s ∆]
  (let [new-h (map + (get-in s [:pos 'H]) ∆)]
    (loop [s (-> s (assoc-in [:pos 'H] new-h) (update-in [:vis 'H] conj new-h))
           [i & r] (rest ks) new-k1 new-h]
      (if-not i
        s
        (let [new-k2 (pull new-k1 (get-in s [:pos i]))]
          (recur (-> s (assoc-in [:pos i] new-k2) (update-in [:vis i] conj new-k2))
                 r new-k2))))))

(comment
  (let [moves (->> #_(slurp "input/2022/09-sample-rope-2.txt")
                   (slurp "input/2022/09-rope-snake.txt")
                   str/split-lines
                   (map #(str "[" % "]"))
                   (map edn/read-string)
                   (mapcat (fn [[d n]] (repeat n (∆s d)))))
        out (loop [s {:pos (zipmap ks (repeat [0 0]))
                      :vis (zipmap ks (repeat [[0 0]]))}
                   [∆ & r] moves]
              (if-not ∆ s (recur (move s ∆) r)))]
    [(count (set (get-in out [:vis '1]))) (count (set (get-in out [:vis '9])))])   ; => [5683 2372]
  )
