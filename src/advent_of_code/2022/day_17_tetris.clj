(ns advent-of-code.2022.day-17-tetris
  (:require [clojure.string :as str]))

(def shapes [{:name \- :right 3 :blocks [[0 0] [1 0] [2 0] [3 0]]}
             {:name \+ :right 2 :blocks [[0 1] [1 0] [1 1] [1 2] [2 1]]}
             {:name \⌟ :right 2 :blocks [[0 0] [1 0] [2 0] [2 1] [2 2]]}
             {:name \| :right 0 :blocks [[0 0] [0 1] [0 2] [0 3]]}
             {:name \■ :right 1 :blocks [[0 0] [1 0] [0 1] [1 1]]}])

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

(defn move [loc blocks h shapes [[jet i] & jets]]
  (let [[n shape] (first shapes)]
    (if-not shape
      {:h h :blocks blocks :loc loc}
      (let [loc (-> loc (blow shape jet blocks) (fall shape blocks))]
        (if (reduced? loc)
          (let [loc (unreduced loc)
                ∆shape (map #(add loc %) (:blocks shape))
                h (reduce (fn [h [_ y]] (max h y)) h ∆shape)
                _ (when (and (= 0 (first loc)) (= \- (:name shape))) (println "i" i "n" n "h" h "loc" loc))]
            (recur [2 (+ h 4)] (into blocks (map vector ∆shape (repeat (:name shape)))) h (rest shapes) jets))
          (recur loc blocks h shapes jets))))))

(defn printout [h blocks]
  (doseq [y (range 60)
          :let [row (for [x (range 7)] (if-let [block (blocks [x (- h y)])] block \.))]]
    (println (str/join row))))

(comment
  ;; part 1 -- ~150msecs
  (time
   (let [jets #_">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
         (slurp "input/2022/17-jet-pattern.txt")
         p [2 3]
         blocks (into {} (for [x (range 7)] [[x -1] \_]))
         shapes (->> shapes cycle (map vector (range)) (take 3500))
         jets (cycle (map vector jets (range)))
         {:keys [h blocks loc]} (move p blocks 0 shapes jets)]
     (printout (+ h 3) blocks)))
  )

;; 2022-12-23
;;
;;    Part 1 not too tricky. For part 2 I need to detect a cycle, which must
;;    happen every i (mod jets). I printed out i every time the \- shape came to
;;    rest in column 0 and determined that when that happens (at jet 104) it
;;    will happen again after 1735 shapes and the height will increase by 2781.
;;    Boom. There is my cycle. I also printed out a few of the topmost rows of
;;    blocks at that point to determine what was the minimum blocks I could keep
;;    as my new "floor" and thus not drag along the entire stack. From that I
;;    saw that the next few shapes block the open gap on the right side letting
;;    me drop everything below the \- shape's current location. (For the sample
;;    puzzle, when i is 37 I can drop everything below, and by the next 37,
;;    we've placed 25 shapes and increased height by 53.)
;;
;;    I'm going to refactor my `move` function to take a parameter `n` for the
;;    number of shapes we want to process and it will count down. But it will
;;    also have a `reset` map so that if we hit the start of a cycle, it
;;    can fast-forward shapes and heights until it comes closer to the desired
;;    n, and then we'll just do normal processing from that point forward.
