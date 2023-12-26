(ns advent-of-code.2023.day-25-wires
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [lines (map #(re-seq #"[a-z]{3}" %) (str/split-lines input))]
    (loop [[[k & r's] & lines] lines verts {} edges []]
      (if-not k
        {:verts verts :edges edges}
        (let [cnt (count edges)
              new's (map-indexed (fn [i r] [(+ i cnt) [k r]]) r's)
              verts (reduce (fn [acc [i [a b]]]
                              (-> acc
                                  (update a (fnil conj #{}) i)
                                  (update b (fnil conj #{}) i)))
                            verts
                            new's)
              edges (into edges (map second) new's)]
          (recur lines verts edges))))))

(defn replace-edge [coll old new]
  (cond-> coll (contains? coll old) (-> (disj old) (conj new))))

(defn replace-vert [[a b] old's new]
  (if (old's a)
    [new b]
    [a new]))

(defn swap-out-edge
  "Swaps edge `edg` at position i with edge `oth` at position j. In the verts
   map, the edges for `oth` are updated to reflect it's new location at i, but
   the same is not done for `edg` (because it's about to be goners)."
  [{:keys [verts edges]} i j]
  (let [[a b :as edg] (nth edges i)
        [c d :as oth] (nth edges j)
        verts (-> verts
                  (update a disj i)
                  (update b disj i)
                  (update c replace-edge j i)
                  (update d replace-edge j i))
        edges (assoc edges i oth j edg)]
    {:verts verts :edges edges}))

(defn select-edge
  "Selects and edge and swaps it with the last. Updates the edge list only for
   what was previously the last edge. What is now the last will be goners."
  [state]
  ;; 1. randomly select n and call (swap-out-edge n (dec (count edges)))
  ;; 2.
  ;; TODO: whatever edge we select, if it shows up multiple times in the edge
  ;; list, those all need to be removed.
  (let [cnt (count (:edges state))
        n (rand-int cnt)]
    (swap-out-edge state n (dec cnt))))

(defn contract [state]
  (let [{:keys [verts edges]} (select-edge state)
        [[a b] edges] ((juxt peek pop) edges)
        newv (into #{} (flatten [a b]))
        newe (into (verts a) (verts b))
        verts (-> verts (dissoc a b) (assoc newv newe))
        edges (reduce (fn [edges n]
                        (update edges n replace-vert #{a b} newv))
                      edges
                      newe)]
    {:verts verts :edges edges}))

(comment
  (def input "aaa bbb ccc ddd\nbbb ccc ddd\nccc ddd")
  (def input (slurp "input/2023/25-sample.txt"))
  (let [state (parse input)]
    (swap-out-edge state 5 5)
    #_(contract state))

  (swap-edge #{31 32 3 30} 31 1)

  )
