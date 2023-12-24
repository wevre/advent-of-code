(ns advent-of-code.2023.day-23-hikes
  (:require [advent-of-code.common2 :as common2]
            [advent-of-code.common :as common]))

(defn find-start [trails row]
  (->> (dissoc trails :size)
       (keep (fn [[[r c] v]] (when (and (= r row) (= v \.)) [r c])))
       first))

(def path-type #{\. \v \^ \< \>})

(defn nigh's [locmap]
  (let [[rows cols] (:size locmap)]
    (fn [loc]
      (for [∆ [[-1 0] [1 0] [0 -1] [0 1]]
            :let [[r c :as nxt] (mapv + loc ∆)]
            :when (and (<= 0 r (dec rows)) (<= 0 c (dec cols)))
            :when (path-type (locmap nxt))]
        nxt))))

(defn ?downhill [nxt loc typ]
  (or (= \. typ)
      (= (mapv - nxt loc) ({\v [1 0] \^ [-1 0] \< [0 -1] \> [0 1]} typ))))

(defn next-branch [locmap nigh's-er]
  (memoize
   (fn [loc frm dst]
     (let [nxt's (->> (nigh's-er loc) (remove #{frm}) (filter #(path-type (locmap %))))]
       (if (not= 1 (count nxt's))
         [loc dst]
         (let [nxt (first nxt's)]
           (if (?downhill nxt loc (locmap nxt))
             (recur nxt loc (inc dst))
             nil)))))))

(defn next's [locmap]
  (let [nigh's-er (nigh's locmap)
        branch-er (next-branch locmap nigh's-er)]
    (fn [{:as node :keys [loc path]}]
      (for [nxt (nigh's-er loc)
            :when (?downhill nxt loc (locmap nxt))
            :let [[nxt dst] (branch-er nxt loc 1)]
            :when (and nxt (not (path nxt)))]
        {:loc nxt :dst (+ (:dst node) dst) :path (assoc path nxt loc)}))))

(defn paths [locmap beg end]
  (let [next's-er (next's locmap)
        node {:loc beg :dst 0 :path {beg nil}}]
    (loop [queue (common/queue node) finalists []]
      (let [[{:as node :keys [loc dst]} queue] ((juxt peek pop) queue)]
        (if node
          (if (= end loc)
            (recur queue (conj finalists dst))
            (recur (into queue (next's-er node)) finalists))
          finalists)))))

(comment
  (do
    (def trails (into {} (common2/locmap<- identity :?size true) (slurp "input/2023/23-sample.txt")))
    (def start (find-start trails 0))
    (def end (find-start trails (dec (second (:size trails))))))
  (do
    (def trails (into {} (common2/locmap<- identity :?size true) (slurp "input/2023/23-hikes.txt")))
    (def start (find-start trails 0))
    (def end (find-start trails (dec (second (:size trails))))))

  ;; year 2023 day 23 puzzle 1
  (reduce max (paths trails start end))
  ;; => 2162

  ;; year 2023 day 23 puzzle 2
  (time
   (reduce max (paths (update-vals trails #(if (path-type %) \. %)) start end)))
  )
