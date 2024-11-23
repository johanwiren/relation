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
  (:refer-clojure :exclude [assoc comp dissoc set seq update extend update sort-by vec]))

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

(defn comp
  ([rel xform]
   (impl/compose rel xform))
  ([rel xform & xfs]
   (reduce #(impl/compose %1 %2) rel (cons xform xfs))))

(defn select
  "Selects rows for which (pred row) returns true."
  [rel pred]
  (comp rel (filter pred)))

(defn assoc
  "Associates key(s) and val(s) to all rows."
  [rel key val & kvs]
  (comp rel (map #(apply core/assoc % key val kvs))))

(defn dissoc
  "Disassociates key(s) from all rows."
  [rel key & keys]
  (comp rel (map #(apply core/dissoc % key keys))))

(defn rename
  "Renames keys on all rows using kmap."
  [rel kmap]
  (comp rel (map #(set/rename-keys % kmap))))

(defn extend
  "Associates k to each row with the value of (f row)"
  ([rel kmap]
   (comp rel (map #(reduce-kv (fn [tuple k f]
                                   (core/assoc tuple k (f tuple)))
                                 %
                                 kmap))))
  ([rel k f]
   (comp rel (map #(core/assoc % k (f %)))))
  ([rel k f & kfs]
   (extend rel (apply hash-map k f kfs))))

(defn update
  "Updates k in each row with the rusult of applying f to the old value."
  [rel k f & args]
  (comp rel (map #(apply core/update % k f args))))

(defn project
  "Keeps only keys ks for each row"
  [rel ks]
  (comp rel (map #(select-keys % ks))))

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
      (comp yrel (join* xrel (set/map-invert kmap)))
      (comp xrel (join* yrel kmap)))))

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
  (comp xrel (left-join* (relation yrel) kmap)))

(defn right-join
  "Same as join but always keep all rows in yrel"
  [xrel yrel kmap]
  (let [yrel (relation yrel)]
    (comp yrel (left-join* xrel (set/map-invert kmap)))))

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
                  (let [new (agg-fn (get-in aggs [by k] (agg-fn)) (key-fn row))]
                    (assoc! aggs by (assoc! (get aggs by (transient {})) k new))))
                aggs
                aggs-map))))
       (transient {})
       (seq rel))
      (persistent!)
      (map (fn [[by row]]
             (merge-with (fn [[agg-fn _] res]
                            (agg-fn res))
                         aggs-map
                         (merge (persistent! row) by))))
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
  agg-fn must be a reducing function with an additional zero arity function
  that produces an initial value.

  Example: (aggregate-by rel {:album/length [+ :song/length]})"
  ([rel aggs-map]
   (aggregate-by rel [] aggs-map))
  ([rel key agg & more]
   (aggregate-by rel [] (apply hash-map key agg more))))

(defn sort-by [rel keyfn]
  (let [by (fn [x y]
             (let [xval (keyfn x)
                   yval (keyfn y)
                   by-val (compare xval yval)]
               (if (zero? by-val)
                 (let [xtype (type x)
                       ytype (type y)
                       by-type (compare xtype ytype)]
                   (if (zero? by-type)
                     (compare (hash x) (hash y))
                     by-type))
                 by-val)))]
    (relation (into (sorted-set-by by) (impl/entries rel)))))

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
                              (core/group-by (core/comp keyword namespace) ks)
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
  (let [yset (impl/set yrel)]
    (if (empty? yset)
      xrel
      (comp xrel (remove (impl/set yrel))))))

(defn intersection
  "Returns a relation that is the intersection of xrel and yrel."
  [xrel yrel]
  (let [yset (impl/set yrel)]
    (if (empty? yset)
      (relation #{})
      (comp xrel (filter yset)))))

(defn stats-agg
  ([] {:max #?(:clj Double/NEGATIVE_INFINITY
               :cljs js/Number.MIN_VALUE)
       :min #?(:clj Double/POSITIVE_INFINITY
               :cljs js/Number.MAX_VALUE)
       :avg 0
       :count 0
       :sum 0})
  ([x] x)
  ([{mn :min mx :max sum :sum cnt :count} y]
   (hash-map :max (max mx y)
             :min (min mn y)
             :sum (+ sum y)
             :count (inc cnt)
             :avg (/ (+ sum y) (inc cnt)))))

(defn extend-stats [rel k]
  (let [ns (namespace k)]
    (comp
     rel
     (map #(merge
            (core/dissoc % k)
            (update-keys
             (k %)
             (fn [stat-k]
               (keyword ns
                        (if (= :count stat-k)
                          (name stat-k)
                          (str (name stat-k) "-" (name k)))))))))))

(defn extend-kv [rel k]
  (let [ns (namespace k)]
    (comp
     rel
     (map #(merge
            (core/dissoc % k)
            (update-keys
             (k %)
             (fn [k']
               (keyword ns (name k')))))))))

(defn expand-kv [rel k]
  (let [ns (namespace k)]
    (comp
     rel
     (mapcat (fn [row]
               (map (fn [[key val]]
                      (core/assoc (core/dissoc row k)
                                  (keyword ns "key") key
                                  (keyword ns "val") val))
                    (k row)))))))

(defn expand-seq [rel k]
  (comp
   rel
   (mapcat (fn [row]
             (map (fn [val]
                    (core/assoc row k val))
                  (k row))))))

(defn conj-agg [ctor]
  (fn
    ([] (ctor))
    ([x] x)
    ([x y] (conj x y))))

(def vec-agg
  "Vector aggregation function.

  Collects all values into a vector."
  (conj-agg vector))

(def set-agg
  "Set aggregation function.

  Collects all values into a set."
  (conj-agg hash-set))

(def count-agg
  "Count aggregation function

  Returns the rowcount."
  [+ (constantly 1)])
