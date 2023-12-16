(ns advent-of-code.2023.day-16-v2
  (:require [advent-of-code.common2 :as common2]))

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

(defn out-of-bounds [[r c] [rows cols]]
  (not (and (< -1 r rows) (< -1 c cols))))

(defn walk [locmap size]
  (fn walk [loc frm to<-from]
    (cond
      (out-of-bounds loc size) (assoc to<-from loc #{frm})
      (get (get to<-from frm #{}) loc) to<-from
      :else
      (let [to<-from (update to<-from frm (fnil conj #{}) loc)
            type (locmap loc \.)
            dir (vec- loc frm)
            next's (get-in nexts<-dir<-type [type dir])]
        (if (= 1 (count next's))
          (recur (vec+ (first next's) loc) loc to<-from)
          (reduce (fn [m nxt] (walk (vec+ nxt loc) loc m)) to<-from next's))))))

(defn energized [size]
  (fn [to<-from]
    (->> to<-from
         (reduce-kv (fn [s k v] (into (conj s k) v)) #{})
         (remove #(out-of-bounds % size))
         count)))

(defn max-energized [locmap [rows cols :as size]]
  (let [energize-er (energized size)
        walk-er (walk locmap size)
        entry's (concat (for [r (range rows)] [[r 0] [r -1]])
                        (for [r (range rows)] [[r (dec cols)] [r cols]])
                        (for [c (range cols)] [[0 c] [-1 c]])
                        (for [c (range cols)] [[(dec rows) c] [rows c]]))]
    (loop [[[loc frm] & rest] entry's exit's #{} maxe 0]
      (cond
        (not loc) maxe
        (exit's frm) (recur rest exit's maxe)
        :else (let [results (walk-er loc frm {})
                    exit's (into exit's (->> results keys (filter #(out-of-bounds % size))))
                    maxe (max maxe (energize-er results))]
                (recur rest exit's maxe))))))

(comment
  (do
    (def layout (into {} (common2/locmap<- #{\\ \/ \- \|} :?size true) (slurp "input/2023/16-sample.txt")))
    (def size (:size layout)))
  (do
    (def layout (into {} (common2/locmap<- #{\\ \/ \- \|} :?size true) (slurp "input/2023/16-mirrors.txt")))
    (def size (:size layout)))

  ;; year 2023 day 16 puzzle 1
  ((energized size) (sort ((walk layout size) [0 0] [0 -1] {})))
  ;; => 7496

  ;; year 2023 day 16 puzzle 2
  (time (max-energized layout size))
  ;; => 7932
  )