(ns advent-of-code.2020.day-20
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def rot-map 
  "If my north is <key>, what are my other directions?"
  {:N {:N :N, :E :E, :S :S, :W :W}
   :E {:N :E, :E :S, :S :W, :W :N}
   :S {:N :S, :E :W, :S :N, :W :E}
   :W {:N :W, :E :N, :S :E, :W :S}
   :n {:N :n, :E :e, :S :s, :W :w}
   :e {:N :e, :E :s, :S :w, :W :n}
   :s {:N :s, :E :w, :S :n, :W :e}
   :w {:N :w, :E :n, :S :e, :W :s}})

(defn find-rot
  "If my <edge> edge is rotated to <type>, what is my north?"
  [edge type]
  (first (keep (fn [[k v]] (when (= type (v edge)) k)) rot-map)))

(defn parse-tile
  "Takes seq of strings defining puzzle (including id string) and returns list
   of key-value pairs (vectors) ready to pour into a map."
  [input]
  (let [[id-str & rest] input
        id (edn/read-string (re-find #"\d+" id-str))
        img (map seq rest)]
    [{:edge (first img) :id id :type :N}
     {:edge (last img) :id id :type :S}
     {:edge (map first img) :id id :type :W}
     {:edge (map last img) :id id :type :E}
     {:edge (reverse (first img)) :id id :type :n}
     {:edge (reverse (last img)) :id id :type :s}
     {:edge (reverse (map first img)) :id id :type :e}
     {:edge (reverse (map last img)) :id id :type :w}]))

(defn puzzle [input]
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (mapcat parse-tile)
       (group-by :edge)
       (reduce-kv (fn [acc _k [{id1 :id t1 :type} {id2 :id t2 :type}]]
                    (cond-> acc
                      id2 (update id1 (fnil conj {}) {t1 {:id id2 :type t2}})
                      id2 (update id2 (fnil conj {}) {t2 {:id id1 :type t1}})))
                  {})
       (filter #(= 4 (count (val %))))
       (map key)
       (apply *)))

(comment
  (puzzle (slurp "input/2020/20-tiles.txt"))

  (puzzle (slurp "input/2020/20-example_tiles.txt")))