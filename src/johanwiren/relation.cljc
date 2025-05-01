(ns johanwiren.relation
  "Composeable Relational algebra operations."
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   [clojure.set :as set])
  (:refer-clojure :exclude [assoc dissoc update extend update sort-by]))

(defn |>
  [relation & xforms]
  (into (empty relation) (reduce comp xforms) relation))

(defn |>seq
  [relation & xforms]
  (sequence (reduce comp xforms) relation))

(defn |>set
  [relation & xforms]
  (into #{} (reduce comp xforms) relation))

(defn |>vec
  [relation & xforms]
  (into [] (reduce comp xforms) relation))

(defn- --first
  ([] nil)
  ([x] x)
  ([_ x] (reduced x)))

(defn |>first
  [relation & xforms]
  (transduce (reduce comp xforms) --first relation))

(declare normalize)

(defn |>normalized
  [relation & forms]
  (transduce (reduce comp forms) normalize relation))

(defn select
  "Selects rows for which (pred row) returns true."
  [pred]
  (filter pred))

(defn select*
  "Selects rows for which (pred row) returns true."
  [coll pred]
  (into (empty coll) (select pred) coll))

(defn assoc
  "Associates key(s) and val(s) to all rows."
  [key val & kvs]
  (map #(apply core/assoc % key val kvs)))

(defn assoc*
  "Associates key(s) and val(s) to all rows."
  [coll key val & kvs]
  (into (empty coll) (apply assoc key val kvs) coll))

(defn dissoc
  "Disassociates key(s) from all rows."
  [key & keys]
  (map #(apply core/dissoc % key keys)))

(defn dissoc*
  "Disassociates key(s) from all rows."
  [coll key & keys]
  (into (empty coll) (apply dissoc key keys) coll))

(defn rename
  "Renames keys on all rows using kmap."
  [kmap]
  (map #(set/rename-keys % kmap)))

(defn rename*
  "Renames keys on all rows using kmap."
  [coll kmap]
  (into (empty coll) (rename kmap) coll))

(defn extend
  "Associates k to each row with the value of (f row)"
  ([kmap]
   (map #(reduce-kv (fn [row k f]
                      (core/assoc row k (f row)))
                    %
                    kmap)))
  ([k f]
   (map #(core/assoc % k (f %))))
  ([k f & kfs]
   (extend (apply hash-map k f kfs))))

(defn extend*
  "Associates k to each row with the value of (f row)"
  ([coll kmap]
   (into (empty coll) (extend kmap) coll))
  ([coll k f]
   (into (empty coll) (extend k f) coll))
  ([coll k f & kfs]
   (into (empty coll) (apply extend k f kfs) coll)))

(defn update
  "Updates k in each row with the rusult of applying f to the old value."
  [k f & args]
  (map #(apply core/update % k f args)))

(defn update*
  "Updates k in each row with the rusult of applying f to the old value."
  [coll k f & args]
  (into (empty coll) (apply update k f args) coll))

(defn project-pred
  "Keeps only keys matching pred for each row."
  [pred]
  (map #(into {} (filter (comp pred key)) %)))

(defn project-pred*
  "Keeps only keys matching pred for each row."
  [coll pred]
  (into (empty coll) (project-pred pred) coll))

(defn project-ns
  "Keeps only keys with matching namespace(s)"
  [namespaces]
  (project-pred (comp (into #{} (map name) namespaces) namespace)))

(defn project-ns*
  "Keeps only keys with matching namespace(s)"
  [coll namespaces]
  (into (empty coll) (project-ns namespaces) coll))

(defn project
  "Keeps only keys ks for each row"
  [ks]
  (map #(select-keys % ks)))

(defn project*
  "Keeps only keys ks for each row"
  [coll ks]
  (into (empty coll) (project ks) coll))

(defn- index
  "Returns a map of distinct values for ks to distinct rows for those values."
  [rel ks]
  (->
   (reduce (fn [acc x]
             (let [idx-key (select-keys x ks)]
               (assoc! acc idx-key (conj! (get acc idx-key (transient #{})) x))))
           (transient {})
           rel)
   persistent!
   (update-vals persistent!)))

(defn- -join [yrel kmap kind]
  (fn [rf]
    (let [idx (index yrel (vals kmap))
          used-idx-keys (when (#{:full :right} kind)
                          (volatile! #{}))
          ks (keys kmap)]
      (fn
        ([] (rf))
        ([res]
         (if (#{:full :right} kind)
           (let [yitems (->> (apply core/dissoc idx @used-idx-keys)
                             (vals)
                             (apply concat))]
             (rf (reduce rf res yitems)))
           (rf res)))
        ([res item]
         (let [idx-key (set/rename-keys (select-keys item ks) kmap)
               found (get idx idx-key)]
           (if found
             (do
               (when (#{:full :right} kind)
                 (vswap! used-idx-keys conj idx-key))
               (reduce rf res (map #(merge item %) found)))
             (if (#{:full :outer :right} kind)
               (rf res item)
               res))))))))

(defn recursive-join
  "Joins relation yrel using the corresponding attributes in join-kmap
  For each found entry in yrel, recursively joins yrel on corresponding
  attributes in recur-kmap.

  Adds :johanwiren.relation/depth to joined entries."
  [yrel join-kmap recur-kmap]
  (fn [rf]
    (let [join-idx (index yrel (vals join-kmap))
          join-ks (keys join-kmap)
          rec-idx (index yrel (vals recur-kmap))
          rec-ks (keys recur-kmap)]
      (fn
        ([] (rf))
        ([res] (rf res))
        ([res item]
         (let [join-idx-key (set/rename-keys (select-keys item join-ks) join-kmap)
               join-found (get join-idx join-idx-key)
               level 0]
           (loop [res (reduce rf res (map #(merge item % {::depth level}) join-found))
                  level (inc level)
                  items join-found]
             (if (seq items)
               (let [idx-key (set/rename-keys (select-keys (first items) rec-ks) recur-kmap)
                     found (get rec-idx idx-key)]
                 (recur
                  (reduce rf res (map #(merge item % {::depth level}) found))
                  (inc level)
                  (into (rest items) found)))
               res))))))))

(defn recursive-join*
  "Joins relation yrel using the corresponding attributes in join-kmap
  For each found entry in yrel, recursively joins yrel on corresponding
  attributes in recur-kmap.

  Adds :johanwiren.relation/depth to joined entries."
  [coll yrel join-kmap recur-kmap]
  (into (empty coll) (recursive-join yrel join-kmap recur-kmap) coll))

(defn- self-join [as kmap kind]
  (fn [rf]
    (let [items (volatile! (transient []))]
      (fn
        ([] (rf))
        ([res]
         (let [items (persistent! @items)
               f (comp
                  (map #(update-keys % (comp (partial keyword as) name)))
                  (-join items kmap kind))]
           (rf (reduce (f rf) res items))))
        ([res item]
         (vswap! items conj! item)
         res)))))

(defn join
  "Joins relation yrel using the corresponding attributes in kmap.

  Keys in yrel will be merged into xrel with yrel taking precedence."
  ([yrel kmap]
   (join yrel kmap :inner))
  ([yrel kmap kind]
   (if (and (qualified-keyword? yrel)
          (= "self" (namespace yrel)))
     (self-join (name yrel) kmap kind)
     (-join yrel kmap kind))))

(defn join*
  "Joins relation yrel using the corresponding attributes in kmap.

  Keys in yrel will be merged into xrel with yrel taking precedence."
  ([coll yrel kmap]
   (join* coll yrel kmap :inner))
  ([coll yrel kmap kind]
   (if (and (qualified-keyword? yrel)
          (= "self" (namespace yrel)))
     (into (empty coll) (self-join (name yrel) kmap kind) coll)
     (into (empty coll) (-join yrel kmap kind) coll))))

(defn left-join
  "Same as join but always keeps all rows in xrel"
  [yrel kmap]
  (join yrel kmap :outer))

(defn left-join*
  "Same as join but always keeps all rows in xrel"
  [coll yrel kmap]
  (into (empty coll) (left-join yrel kmap) coll))

(defn right-join
  "Same as join but always keep all rows in yrel"
  [yrel kmap]
  (join yrel kmap :right))

(defn right-join*
  "Same as join but always keep all rows in yrel"
  [coll yrel kmap]
  (into (empty coll) (right-join yrel kmap) coll))

(defn full-join [yrel kmap]
  (join yrel kmap :full))

(defn full-join* [coll yrel kmap]
  (into (empty coll) (full-join yrel kmap) coll))

(defn anti-join [yrel kmap]
  (comp
   (left-join yrel kmap)
   (select (apply every-pred (map #(comp nil? %) (vals kmap))))))

(defn anti-join* [coll yrel kmap]
  (into (empty coll) (anti-join yrel kmap) coll))

(defn aggregate-by
  "Returns an aggregated relation grouped by ks using aggs-map.
  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function.

  Example: (aggregate-by [:album/name] {:album/length [+ :song/length]})"
  ([ks aggs-map]
   (let [ks (if (keyword ks) [ks] ks)]
     (fn [rf]
       (let [aggs (volatile! {})]
         (fn
           ([] (rf))
           ([res]
            (->> @aggs
                 (map (fn [[by row]]
                        (merge-with (fn [[agg-fn _] res]
                                      (agg-fn res))
                                    aggs-map
                                    (merge row by))))
                 (reduce rf res)
                 rf))
           ([res item]
            (let [by (select-keys item ks)]
              (vreset!
               aggs
               (reduce-kv (fn [aggs' k [agg-fn key-fn]]
                            (let [new (agg-fn (get-in aggs' [by k] (agg-fn)) (key-fn item))]
                              (core/assoc aggs'
                                          by
                                          (core/assoc (get aggs' by {})
                                                      k
                                                      new))))
                          @aggs
                           aggs-map)))
            res))))))
  ([ks key agg & more]
   (aggregate-by ks (apply hash-map key agg more))))

(defn aggregate-by*
  "Returns an aggregated relation grouped by ks using aggs-map.
  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function.

  Example: (aggregate-by coll [:album/name] {:album/length [+ :song/length]})"
  ([coll ks aggs-map]
   (into (empty coll) (aggregate-by ks aggs-map) coll))
  ([coll ks key agg & more]
   (aggregate-by* coll ks (apply hash-map key agg more))))

(defn aggregate-over
  "Returns a relation with aggregations joined into rel.
  See aggregate-by"
  ([ks aggs-map]
   (let [ks (if (keyword ks) [ks] ks)]
     (fn [rf]
       (let [aggs (volatile! {})
             idx (volatile! {})]
         (fn
           ([] (rf))
           ([res]
            (->> @aggs
                 (map (fn [[by row]]
                        (merge-with (fn [[agg-fn _] res]
                                      (agg-fn res))
                                    aggs-map
                                    (merge row by))))
                 (mapcat (fn [row]
                           (->> (get @idx (select-keys row ks))
                                (map #(merge % row)))))
                 (reduce rf res)
                 rf))
           ([res item]
            (let [by (select-keys item ks)]
              (vswap! idx core/update by (fnil conj []) item)
              (vreset!
               aggs
               (reduce-kv (fn [aggs' k [agg-fn key-fn]]
                            (let [new (agg-fn (get-in aggs' [by k] (agg-fn)) (key-fn item))]
                              (core/assoc aggs'
                                          by
                                          (core/assoc (get aggs' by {})
                                                      k
                                                      new))))
                          @aggs
                          aggs-map)))
            res))))))
  ([ks key agg & more]
   (aggregate-over ks (apply hash-map key agg more))))

(defn aggregate-over*
  "Returns a relation with aggregations joined into rel.
  See aggregate-by"
  ([coll ks aggs-map]
   (into (empty coll) (aggregate-over ks aggs-map) coll))
  ([coll ks key agg & more]
   (aggregate-over* coll ks (apply hash-map key agg more))))

(defn aggregate
  "Returns an aggregated relation.
  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function with an additional zero arity function
  that produces an initial value.

  Example: (aggregate {:album/length [+ :song/length]})"
  ([aggs-map]
   (aggregate-by [] aggs-map))
  ([key agg & more]
   (aggregate-by [] (apply hash-map key agg more))))

(defn aggregate*
  "Returns an aggregated relation.
  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function with an additional zero arity function
  that produces an initial value.

  Example: (aggregate coll {:album/length [+ :song/length]})"
  ([coll aggs-map]
   (into (empty coll) (aggregate-by [] aggs-map) coll))
  ([coll key agg & more]
   (aggregate* coll (apply hash-map key agg more))))

(defn sort-by [keyfn]
  (fn [rf]
    (let [items (volatile! (transient []))]
      (fn
        ([] (rf))
        ([res]
         (->> @items
              persistent!
              (core/sort-by keyfn)
              (reduce rf res)
              rf))
        ([res item]
         (vswap! items conj! item)
         res)))))

(defn sort-by*
  "Sorts using key-fn. Returns a vector."
  [coll key-fn]
  (into [] (sort-by key-fn) coll))

(defn- normalize
  "Normalizes a relation.
  Returns a map of namespace to distinct maps with keys for only that namespace."
  ([]
   {:kmap {}
    :ks #{}
    :relmap (transient {})})
  ([{:keys [relmap]}]
   (-> relmap
       persistent!
       (update-vals persistent!)))
  ([{:keys [relmap ks kmap]} row]
   (let [rel-keys (keys row)
         update-ks? (not-every? ks rel-keys)
         ks (if update-ks? (into ks (keys row)) ks)
         kmap (if update-ks?
                (group-by (comp keyword namespace) ks)
                kmap)]
     {:kmap kmap
      :ks ks
      :relmap
      (reduce-kv (fn [relmap relvar ks]
                   (let [selected (select-keys row ks)]
                     (if (seq selected)
                       (core/assoc! relmap
                                    relvar
                                    (conj! (get relmap relvar (transient #{}))
                                           selected))
                       relmap)))
                 relmap
                 kmap)})))

(defn normalize*
  [coll]
  (transduce (map identity) normalize coll))

(defn union
  "Returns a relation that is the union of xrel and yrel."
  [yrel]
  (comp
   (fn [rf]
     (fn
       ([] (rf))
       ([res] (rf (reduce rf res yrel)))
       ([res item] (rf res item))))
   (distinct)))

(defn union*
  "Returns a relation that is the union of xrel and yrel."
  [coll yrel]
  (into (empty coll) (union yrel) coll))

(defn union-all
  [yrel]
  (fn [rf]
    (fn
      ([] (rf))
      ([res] (rf (reduce rf res yrel)))
      ([res item] (rf res item)))))

(defn union-all*
  [coll yrel]
  (into (empty coll) (union-all yrel) coll))

(defn difference
  "Returns a relation that is xrel without the elemens in yrel."
  [yrel]
  (comp (remove (set yrel))
        (distinct)))

(defn difference*
  "Returns a relation that is xrel without the elemens in yrel."
  [coll yrel]
  (into (empty coll) (difference yrel) coll))

(defn intersection
  "Returns a relation that is the intersection of xrel and yrel."
  [yrel]
  (comp (filter (set yrel))
        (distinct)))

(defn intersection*
  "Returns a relation that is the intersection of xrel and yrel."
  [coll yrel]
  (into (empty coll) (intersection yrel) coll))

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

(defn extend-stats [k]
  (let [ns (namespace k)]
    (map #(merge
           (core/dissoc % k)
           (update-keys
            (k %)
            (fn [stat-k]
              (keyword ns
                       (if (= :count stat-k)
                         (name stat-k)
                         (str (name stat-k) "-" (name k))))))))))

(defn extend-stats*
  [coll k]
  (into (empty coll) (extend-stats k) coll))

(defn extend-kv [k]
  (let [ns (namespace k)]
    (map #(merge
           (core/dissoc % k)
           (update-keys
            (k %)
            (fn [k']
              (keyword ns (name k'))))))))

(defn extend-kv* [coll k]
  (into (empty coll) (extend-kv k) coll))

(defn expand-kv [k]
  (let [ns (namespace k)]
    (mapcat (fn [row]
              (map (fn [[key val]]
                     (core/assoc (core/dissoc row k)
                                 (keyword ns "key") key
                                 (keyword ns "val") val))
                   (k row))))))

(defn expand-kv* [coll k]
  (into (empty coll) (expand-kv k) coll))

(defn expand-seq [k]
  (mapcat (fn [row]
            (map (fn [val]
                   (core/assoc row k val))
                 (k row)))))

(defn expand-seq* [coll k]
  (into (empty coll) (expand-seq k) coll))

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

