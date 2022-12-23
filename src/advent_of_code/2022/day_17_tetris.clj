(ns advent-of-code.2022.day-17-tetris)

(def shapes [{:name :- :right 3 :blocks [[0 0] [1 0] [2 0] [3 0]]}
             {:name :+ :right 2 :blocks [[0 1] [1 0] [1 2] [2 1]]}
             {:name :⌟ :right 2 :blocks [[0 0] [1 0] [2 0] [2 1] [2 2]]}
             {:name :| :right 0 :blocks [[0 0] [0 1] [0 2] [0 3]]}
             {:name :■ :right 1 :blocks [[0 0] [1 0] [0 1] [1 1]]}])

(defn add [& ps] (apply mapv + ps))

(defn blow [[x _ :as p] shape jet blocks]
  (let [∆x ({\< -1 \> +1} jet)
        ∆shape (mapv #(add [∆x 0] p %) (:blocks shape))]
    (cond-> p
      (not (or (some blocks ∆shape) (not (<= 0 (+ x ∆x) (- 6 (:right shape))))))
      (add [∆x 0]))))

(defn fall [p shape blocks]
  (let [∆p (add p [0 -1])
        ∆shape (mapv #(add ∆p %) (:blocks shape))]
    (if (some blocks ∆shape)
      (reduced p)
      ∆p)))

(defn move [loc blocks h shapes [jet & jets]]
  (let [shape (first shapes)]
    (if-not shape
      h
      (let [loc (-> loc (blow shape jet blocks) (fall shape blocks))]
        (if (reduced? loc)
          (let [loc (unreduced loc)
                ∆shape (map #(add loc %) (:blocks shape))
                h (reduce (fn [h [_ y]] (max h y)) h ∆shape)]
            (recur [2 (+ h 4)] (into blocks ∆shape) h (rest shapes) jets))
          (recur loc blocks h shapes jets))))))

(comment
  ;; part 1 -- ~150msecs
  (time
   (let [jets #_">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
         (slurp "input/2022/17-jet-pattern.txt")
         p [2 3]
         blocks (into #{} (for [x (range 7)] [x -1]))]
     (move p blocks 0 (->> shapes cycle (take 2022)) (cycle jets))))
  )
