(ns advent-of-code.2022.day-22-monkey-map
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; NOTE: What I'm calling 'board space' is the original columns and rows of the
;;    input map, (although I'm indexing from 0, not 1 as in the problem
;;    description); 'layout' space is measured in coordinates of size `dim` from
;;    upper-left corner of map, giving location of faces; 'face' space is
;;    position within a face (from 0,0 in upper-left corner of face).

(def txf
  "These triplets will be multiplied by [row col (dim-1)] to determine new rows
   and cols as we cross from one face to another."
  {:row  [1 0 0]    ; same row
   :col  [0 1 0]    ; same col
   :cmpc [0 -1 1]   ; complement of col: c --> dim-1-c
   :cmpr [-1 0 1]   ; complement of row: r --> dim-1-r
   })

(defn- add [& ps] (apply mapv + ps))

(defn- mult [& ps] (apply mapv * ps))

(def ∆s {:up [-1 0] :down [+1 0] :left [0 -1] :right [0 +1]})

(comment
  ;; To test layout matches pen and paper drawing.
  (let [layout (edn/read-string (slurp "input/2022/22-monkey-layout-puzzle.edn"))
        dim 4
        shape :fold]
    (for [from-face (range 6)
          [from-dir pos] (map vector [:up :down :left :right] (cycle [[0 0] [(dec dim) (dec dim)]]))
          :let [{to-face :face to-dir :dir row-fn :row col-fn :col} (get-in layout [from-face shape from-dir])
                basis (conj pos (dec dim))
                row (apply + (mult basis (txf row-fn)))
                col (apply + (mult basis (txf col-fn)))]]
      ["from" [pos from-face from-dir] "to" [[row col] to-face to-dir]]))
  )

(def rotate {:up {"R" :right "L" :left}
             :down {"R" :left "L" :right}
             :left {"R" :up "L" :down}
             :right {"R" :down "L" :up}})

(defn- wrapper [locmap layout shape dim]
  (let [face<- (reduce (fn [m [i {l :layout}]] (assoc m l i)) {} layout)]
    (fn [pos dir]
      (let [to-pos (add pos (∆s dir))]
        (if-let [tile (locmap to-pos)]
          [tile to-pos dir]
          (let [face (face<- (mapv #(quot % dim) pos))

                {to-face :face to-dir :dir row-fn :row col-fn :col}
                (get-in layout [face shape dir])

                face-pos (mapv #(rem % dim) pos)
                basis (conj face-pos (dec dim))
                row (apply + (mult basis (txf row-fn)))
                col (apply + (mult basis (txf col-fn)))
                new-pos (add (mult (get-in layout [to-face :layout]) [dim dim])
                             [row col])]
            [(locmap new-pos) new-pos to-dir]))))))

(defn mover [locmap layout shape dim]
  (let [wrap (wrapper locmap layout shape dim)]
    (fn [n pos dir]
      (if (not (pos? n))
        [pos dir]
        (let [[tile to-pos to-dir] (wrap pos dir)]
          (if (= tile \#)
            [pos dir]
            (recur (dec n) to-pos to-dir)))))))

(defn path [locmap layout shape dim]
  (let [move (mover locmap layout shape dim)]
    (fn [pos dir [m & ms]]
      (cond
        (nil? m) [pos dir]
        (integer? m) (let [[pos dir] (move m pos dir)] (recur pos dir ms))
        :else (recur pos (get-in rotate [dir m]) ms)))))

(defn solve [dim face shape layout input]
  (let [layout (edn/read-string (slurp layout))
        pos (mult [dim dim] (get-in layout [face :layout]))
        [tiles moves] (str/split (slurp input) #"\n\n")
        {:keys [locmap]} (common/locmap<- tiles)
        locmap (reduce-kv (fn [m k v] (if (= v \space) m (assoc m k v))) {} locmap)
        moves (map (fn [m f] (f m)) (re-seq #"\d+|[RL]" moves) (cycle [parse-long identity]))
        [[row col] dir] ((path locmap layout shape dim) pos :right moves)]
    (+ (* 1000 (inc row)) (* 4 (inc col)) ({:up 3 :down 1 :left 2 :right 0} dir))))

(comment
  ;; sample puzzle 1
  (solve 4 0 :flat "input/2022/22-monkey-layout-sample.edn" "input/2022/22-monkey-map-sample.txt")   ; => 6032

  ;; sample puzzle 2
  (solve 4 0 :fold "input/2022/22-monkey-layout-sample.edn" "input/2022/22-monkey-map-sample.txt")   ; => 5031

  ;; puzzle 1
  (solve 50 0 :flat "input/2022/22-monkey-layout-puzzle.edn" "input/2022/22-monkey-map.txt")   ; => 64256

  ;; puzzle 1
  (solve 50 0 :fold "input/2022/22-monkey-layout-puzzle.edn" "input/2022/22-monkey-map.txt")   ; => 109224
  )

;; NOTES
;;
;;    I had a hunch that this would turn into a 3D problem for part 2, and I was
;;    right. I tried to design my solution for part 1 so that it wouldn't be
;;    (too) hard to adapt to part 2.
;;
;;    I did _not_ create a general 'fold-this-map' algorithm. Not sure if anyone
;;    else actually did that. Yikes. I just looked at the puzzle data and
;;    created a layout map that captured how edges would come together and how
;;    the direction would change as you crossed over the edges. For the flat map
;;    those transitions are simple (and identical for all six faces). For the
;;    fold version of things it is a little more complicated.
