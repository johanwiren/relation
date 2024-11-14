(ns johanwiren.se.relation
  "(Somewhat) composeable Relational algebra operations.

  A relation that can be threaded through most operations in this
  namespace which composes a transducing process that often yields
  better perfomance than using the corresponding functions in clojure.set

  To realise a the composed process, use either set or seq"
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   [clojure.set :as set]
   [johanwiren.se.relation.impl :as impl])
  (:refer-clojure :exclude [assoc dissoc set seq update extend update sort-by vec]))

(defmacro |>
  "Convenience threading macro similar to -> that realises into a set."
  {:clj-kondo/lint-as 'clojure.core/->}
  [& forms]
  `(impl/set (-> ~@forms)))

(defn relation? [x]
  (impl/relation? x))

(defn relation
  "Creates a relation.
  rel should be a set/sequence of maps."
  [rel]
  (cond
    (relation? rel)
    rel

    (and (seqable? rel)
         (empty? rel))
    (impl/->Relation identity #{})

    (and (set? rel)
         (map? (first rel)))
    (impl/->Relation identity rel)

    (and (seqable? rel)
         (map? (first rel)))
    (impl/->Relation identity (core/set rel))

    :else
    (throw (#?(:clj IllegalArgumentException. :cljs js/Error.)
            "Relations must be a set/seq of maps"))))

(defn vec
  "Realises into a (distinct) vector"
  [rel]
  (impl/vec rel))

(defn seq
  "Realises into a (distinct) sequence."
  [rel]
  (impl/seq rel))

(defn set
  "Realises into a set."
  [rel]
  (impl/set rel))

(defn compose [rel xform]
  (impl/compose rel xform))

(defn select
  "Selects rows for which (pred row) returns true."
  [rel pred]
  (compose rel (filter pred)))

(defn assoc
  "Associates key(s) and val(s) to all rows."
  [rel key val & kvs]
  (compose rel (map #(apply core/assoc % key val kvs))))

(defn dissoc
  "Disassociates key(s) from all rows."
  [rel key & keys]
  (compose rel (map #(apply core/dissoc % key keys))))

(defn rename
  "Renames keys on all rows using kmap."
  [rel kmap]
  (compose rel (map #(set/rename-keys % kmap))))

(defn extend
  "Associates k to each row with the value of (f row)"
  ([rel kmap]
   (compose rel (map #(reduce-kv (fn [tuple k f]
                                   (core/assoc tuple k (f tuple)))
                                 %
                                 kmap))))
  ([rel k f]
   (compose rel (map #(core/assoc % k (f %)))))
  ([rel k f & kfs]
   (extend rel (apply hash-map k f kfs))))

(defn update
  "Updates k in each row with the rusult of applying f to the old value."
  [rel k f & args]
  (compose rel (map #(apply core/update % k f args))))

(defn project
  "Keeps only keys ks for each row"
  [rel ks]
  (compose rel (map #(select-keys % ks))))

(defn index
  "Returns a map of distinct values for ks to distinct rows for those values.

  Realises rel."
  [rel ks]
  (->
   (reduce (fn [acc x]
             (let [idx-key (select-keys x ks)]
               (core/assoc! acc idx-key (conj! (get acc idx-key (transient #{})) x))))
           (transient {})
           (impl/entries rel))
   persistent!
   (update-vals persistent!)))

(defn- join* [yrel kmap]
  (fn [rf]
    (let [idx (index yrel (vals kmap))]
      (fn
        ([] (rf))
        ([res] (rf res))
        ([res item]
         (let [found (get idx (set/rename-keys (select-keys item (keys kmap)) kmap))]
           (if found
             (reduce rf res (map #(merge item %) found))
             res)))))))

(defn join
  "Joins relation yrel using the corresponding attributes in kmap. "
  [xrel yrel kmap]
  (let [yrel (relation yrel)]
    (if (<= (count (impl/keys xrel)) (count (impl/keys yrel)))
      (compose yrel (join* xrel (set/map-invert kmap)))
      (compose xrel (join* yrel kmap)))))

(defn- left-join* [yrel kmap]
  (fn [rf]
    (let [idx (index yrel (vals kmap))]
      (fn
        ([] (rf))
        ([res] (rf res))
        ([res item]
         (let [found (get idx (set/rename-keys (select-keys item (keys kmap)) kmap))]
           (if found
             (reduce rf res (map #(merge item %) found))
             (rf res item))))))))

(defn left-join
  "Same as join but always keeps all rows in xrel"
  [xrel yrel kmap]
  (compose xrel (left-join* (relation yrel) kmap)))

(defn right-join
  "Same as join but always keep all rows in yrel"
  [xrel yrel kmap]
  (let [yrel (relation yrel)]
    (compose yrel (left-join* xrel (set/map-invert kmap)))))

(defn aggregate-by
  "Returns an aggregated relation grouped by ks using aggs-map.
  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function.

  Example: (aggregate-by rel [:album/name] {:album/length [+ :song/length]})"
  ([rel ks aggs-map]
   (let [ks (if (keyword ks) [ks] ks)]
     (->>
      (reduce
       (fn [aggs row]
         (let [by (select-keys row ks)]
           (-> (reduce-kv
                (fn [aggs k [agg-fn key-fn]]
                  (let [cur (get-in aggs [by k])
                        new (if cur
                              (agg-fn cur (key-fn row))
                              (agg-fn (key-fn row)))]
                    (assoc-in aggs [by k] new)))
                aggs
                aggs-map))))
       {}
       (seq rel))
      (map (fn [[by rel]] (merge rel by)))
      relation)))
  ([rel ks key agg & more]
   (aggregate-by rel ks (apply hash-map key agg more))))

(defn aggregate-over
  "Returns a relation with aggregations joined into rel.
  See aggregate-by"
  ([rel ks aggs-map]
   (join rel
         (aggregate-by rel ks aggs-map)
         (into {} (map #(vector % %) (if (keyword? ks) [ks] ks)))))
  ([rel ks key agg & more]
   (aggregate-over rel ks (apply hash-map key agg more))))

(defn aggregate
  "Returns an aggregated relation.
  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function.

  Example: (aggregate-by rel {:album/length [+ :song/length]})"
  ([rel aggs-map]
   (aggregate-by rel [] aggs-map))
  ([rel key agg & more]
   (aggregate-by rel [] (apply hash-map key agg more))))

(defn sort-by [rel keyfn]
  (relation (into (sorted-set-by #(compare (keyfn %1) (keyfn %2))) (impl/entries rel))))

(defn normalize
  "Normalizes a relation.
  Returns a map of namespace to distinct maps with keys for only that namespace.

  Realises rel."
  [rel]
  (->> rel
       impl/entries
       (reduce (fn [{:keys [relmap ks kmap]} rel]
                 (let [rel-keys (keys rel)
                       update-ks? (not-every? ks rel-keys)
                       ks (if update-ks? (into ks (keys rel)) ks)
                       kmap (if update-ks?
                              (core/group-by (comp keyword namespace) ks)
                              kmap)]
                   {:kmap kmap
                    :ks ks
                    :relmap
                    (reduce-kv (fn [relmap relvar ks]
                                 (let [selected (select-keys rel ks)]
                                   (if (core/seq selected)
                                     (core/assoc! relmap
                                                  relvar
                                                  (conj (get relmap relvar #{})
                                                        selected))
                                     relmap)))
                               relmap
                               kmap)}))
               {:kmap {}
                :ks #{}
                :relmap (transient {})})
       :relmap
       (persistent!)))

(defn union
  "Returns a relation that is the union of xrel and yrel."
  [xrel yrel]
  (relation (into (impl/set yrel) (impl/entries xrel))))

(defn difference
  "Returns a relation that is xrel without the elemens in yrel."
  [xrel yrel]
  (compose xrel (remove (impl/set yrel))))

(defn intersection
  "Returns a relation that is the intersection of xrel and yrel."
  [xrel yrel]
  (compose xrel (filter (impl/set yrel))))

#?(:clj
   (defn avg-agg
     "Average aggregation function.

  Returns the average as a ratio"
     ([x]
      (clojure.lang.Ratio. (biginteger x) (biginteger 1)))
     ([x y]
      (clojure.lang.Ratio. (biginteger (+ (numerator x) y))
                           (biginteger (inc (denominator x)))))))

(defn vec-agg
  "Vector aggregation function.

  Collects all values into a vector."
  ([x] (vector x))
  ([x y] (conj x y)))

(defn set-agg
  "Set aggregation function.

  Collects all values into a set."
  ([x] (hash-set x))
  ([x y] (conj x y)))

(def count-agg
  "Count aggregation function

  Returns the rowcount."
  [+ (constantly 1)])
