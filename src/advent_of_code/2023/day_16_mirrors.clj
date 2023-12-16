(ns advent-of-code.2023.day-16-mirrors
  (:require [advent-of-code.common2 :as common2]))

;; I think I need to keep track of a to<-from map. {[r c] #{[ra ca] [rb cb]}}
;; where for a particular loc, we can see the loc(s) where the light beam came
;; from. This will help us detect loops.

;; Then we also need to keep track of energized locs

(def vec+ (partial mapv +))
(def vec- (partial mapv -))

(def U [-1 0])
(def D [1 0])
(def R [0 1])
(def L [0 -1])

(def nexts<-dir<-type
  {\| {R [D U], L [D U], D [D],   U [U]}
   \- {R [R],   L [L],   D [R L], U [R L]}
   \\ {R [D],   L [U],   D [R],   U [L]}
   \/ {R [U],   L [D],   D [L],   U [R]}
   \. {R [R],   L [L],   D [D],   U [U]}})

(defn get-nexts [locmap loc frm]
  (let [type (locmap loc \.)
        dir (vec- loc frm)]
    (get (get nexts<-dir<-type type) dir)))

(defn out-of-bounds [[r c] [rows cols]]
  (not (and (< -1 r rows) (< -1 c cols))))

(defn walk [locmap]
  (let [size (:size locmap)]
    (fn walk [loc frm to<-from]
      (if (or (out-of-bounds loc size) (get (get to<-from frm #{}) loc))
        to<-from
        (let [to<-from (update to<-from frm (fnil conj #{}) loc)
              next's (get-nexts locmap loc frm)]
          (if (= 1 (count next's))
            (recur (vec+ (first next's) loc) loc to<-from)
            (reduce (fn [m nxt] (walk (vec+ nxt loc) loc m)) to<-from next's)))))))

(defn energized [to<-from]
  (dec   ; Ha! because we have [0 -1] (or some other initial OOB loc) in the map.
   (count
    (reduce-kv (fn [i k v] (into (conj i k) v))
               #{}
               to<-from))))

(comment
  (do
    (def layout (into {} (common2/locmap<- #{\\ \/ \- \|} :?size true) (slurp "input/2023/16-sample.txt")))
    (def size (:size layout)))
  (do
    (def layout (into {} (common2/locmap<- #{\\ \/ \- \|} :?size true) (slurp "input/2023/16-mirrors.txt")))
    (def size (:size layout)))

  ;; year 2023 day 16 puzzle 1
  (energized ((walk layout) [0 0] [0 -1] {}))
  ;; => 7496

  ;; year 2023 day 16 puzzle 2
  (time
   (let [[rows cols] size]
     (->>
      (concat (for [r (range rows)] (energized ((walk layout) [r 0] [r -1] {})))
              (for [r (range rows)] (energized ((walk layout) [r (dec cols)] [r cols] {})))
              (for [c (range cols)] (energized ((walk layout) [0 c] [-1 c] {})))
              (for [c (range cols)] (energized ((walk layout) [(dec rows) c] [rows c] {}))))
      (reduce max))))
  ;; => 7932
  )
