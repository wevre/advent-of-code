(ns advent-of-code.2021.day-21)

(def start-1 6)
(def start-2 8)

(defn wrap-10 [x] (inc (mod (dec x) 10)))

(defn deterministic
  "The scores switch each step, most recent turn in first position."
  ([p1 p2] (deterministic p1 p2 0 0 (map #(inc (mod % 100)) (range))))
  ([p1 p2 sc1 sc2 [r1 r2 r3 & dice]]
   (lazy-seq
    (let [p1 (wrap-10 (+ p1 r1 r2 r3))
          sc1 (+ sc1 p1)]
      (cons [sc1 sc2] (deterministic p2 p1 sc2 sc1 dice))))))

(def quantum
  (memoize
   (fn
     ([p1 p2] (quantum p1 p2 0 0))
     ([p1 p2 sc1 sc2]
      (if (<= 21 sc2)
        [0 1]
        (reduce #(mapv + %1 %2)
                (for [r1 [1 2 3] r2 [1 2 3] r3 [1 2 3]
                      :let [p1 (wrap-10 (+ p1 r1 r2 r3))
                            sc1 (+ sc1 p1)]]
                  (reverse (quantum p2 p1 sc2 sc1)))))))))

(comment
  ;; part 1
  (let [[rolls [[_win lose] & _rest]]
        (split-with #(< (first %) 1000) (deterministic start-1 start-2))]
    (* 3 (inc (count rolls)) lose))

  ;; part 2
  (time
   (apply max (quantum start-1 start-2)))
  )
