(ns johanwiren.relation
  "Composeable Relational algebra operations."
  (:require
   #?(:clj [clojure.core :as core]
      :cljs [cljs.core :as core])
   [clojure.set :as set])
  (:refer-clojure :exclude [assoc count dissoc update extend update sort-by]))

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

(defn |>eduction
  [relation & xforms]
  (eduction (reduce comp xforms) relation))

(defn- --first
  ([] nil)
  ([x] x)
  ([_ x] (reduced x)))

(defn |>first
  [relation & xforms]
  (transduce (reduce comp xforms) --first relation))

(defn- --last []
  (let [last (volatile! nil)]
    (fn
      ([] nil)
      ([_] @last)
      ([res x] (vreset! last x) res))))

(defn |>last
  [relation & xforms]
  (transduce (reduce comp xforms) (--last) relation))

(declare normalize)

(defn |>normalized
  [relation & forms]
  (transduce (reduce comp forms) normalize relation))

(defn select
  "Selects rows for which (pred row) returns true."
  [pred]
  (filter pred))

(defn assoc
  "Associates key(s) and val(s) to all rows."
  [key val & kvs]
  (map #(apply core/assoc % key val kvs)))

(defn dissoc
  "Disassociates key(s) from all rows."
  [key & keys]
  (map #(apply core/dissoc % key keys)))

(defn rename
  "Renames keys on all rows using kmap."
  [kmap]
  (map #(set/rename-keys % kmap)))

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

(defn update
  "Updates k in each row with the rusult of applying f to the old value."
  [k f & args]
  (map #(apply core/update % k f args)))

(defn project-pred
  "Keeps only keys matching pred for each row."
  [pred]
  (map #(into {} (filter (comp pred key)) %)))

(defn project-ns
  "Keep only keys with matching namespace(s) for each row."
  [namespaces]
  (project-pred (comp (into #{} (map name) namespaces) namespace)))

(defn project
  "Keeps only keys ks for each row."
  [ks]
  (map #(select-keys % ks)))

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

(defn- -natural-join
  [rel]
  (fn [rf]
    (let [idx (volatile! {})
          ks (volatile! #{})]
      (fn
        ([] (rf))
        ([res] (rf res))
        ([res item]
         (when (empty? @ks)
           (let [ks' (set/intersection (set (keys (first rel))) (set (keys item)))]
             (vreset! ks ks')
             (vreset! idx (index rel ks'))))
         (let [found (get @idx (select-keys item @ks))]
           (reduce rf res (map #(merge item %) found))))))))

(defn- -join
  ([rel kind join-kmap]
   (-join rel kind join-kmap nil))
  ([rel kind join-kmap recur-kmap]
   (fn [rf]
     (let [join-idx (index rel (vals join-kmap))
           join-ks (keys join-kmap)
           used-idx-keys (when (#{:full :right} kind)
                           (volatile! #{}))
           rec-idx (index rel (vals recur-kmap))
           rec-ks (keys recur-kmap)]
       (fn
         ([] (rf))
         ([res]
          (if (#{:full :right} kind)
            (let [yitems (->> (apply core/dissoc join-idx @used-idx-keys)
                              (vals)
                              (apply concat))]
              (rf (reduce rf res yitems)))
            (rf res)))
         ([res item]
          (let [join-idx-key (set/rename-keys (select-keys item join-ks) join-kmap)
                join-found (get join-idx join-idx-key)]
            (if join-found
              (do
                (when (#{:full :right} kind)
                  (vswap! used-idx-keys conj join-idx-key))
                (if (seq recur-kmap)
                  (let [level 0]
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
                        res)))
                  (reduce rf res (map #(merge item %) join-found))))
              (if (#{:full :outer :right} kind)
                (rf res item)
                res)))))))))

(defn as
  "Qualifies all keys in row with given namespace.

  namespace must be a keyword, string or symbol"
  [namespace]
  (let [namespace (name namespace)]
    (map (fn [row]
           (update-keys row #(keyword namespace (name %)))))))

(defn- self-join [as' kmap recur-kmap kind]
  (fn [rf]
    (let [items (volatile! (transient []))]
      (fn
        ([] (rf))
        ([res]
         (let [items (persistent! @items)
               f (comp
                  (as as')
                  (-join items kind kmap recur-kmap))]
           (rf (reduce (f rf) res items))))
        ([res item]
         (vswap! items conj! item)
         res)))))

(defn join
  "Joins with relation rel using the corresponding attributes in kmap.

  When recur-kmap is specified, for each found entry in rel, recursively joins
  rel on corresponding attributes in recur-kmap.
  Adds :johanwiren.relation/depth to joined entries.

  Keys in rel will be merged into the matched row with rel taking precedence."
  ([rel]
   (-natural-join rel))
  ([rel kmap]
   (join rel kmap nil :inner))
  ([rel kmap recur-kmap]
   (join rel kmap recur-kmap :inner))
  ([rel kmap recur-kmap kind]
   (if (and (qualified-keyword? rel)
          (= "self" (namespace rel)))
     (self-join (name rel) kmap recur-kmap kind)
     (-join rel kind kmap recur-kmap))))

(defn left-join
  "Same as join but always keeps all unmatched rows."
  ([rel kmap]
   (left-join rel kmap nil))
  ([rel kmap recur-kmap]
   (join rel kmap recur-kmap :outer)))

(defn right-join
  "Same as join but always keep all rows in rel"
  ([rel kmap]
   (right-join rel kmap nil))
  ([rel kmap recur-kmap]
   (join rel kmap recur-kmap :right)))

(defn full-join
  ([rel kmap]
   (full-join rel kmap nil :full))
  ([rel kmap recur-kmap kind]
   (join rel kmap recur-kmap kind)))

(defn anti-join [rel kmap]
  (comp
   (left-join rel kmap)
   (select (apply every-pred (map #(comp nil? %) (vals kmap))))))

(defn- -aggs-reducer [row]
  (fn [aggs ks aggs-map]
    (let [by (select-keys row (if (keyword? ks) [ks] ks))]
      (reduce-kv (fn [aggs' k [agg-fn key-fn]]
                   (let [new (agg-fn (get-in aggs' [by k] (agg-fn)) (key-fn row))]
                     (core/assoc aggs'
                                 by
                                 (core/assoc (get aggs' by {::by ks})
                                             k
                                             new))))
                 aggs
                 aggs-map))))

(defn- -complete-aggs [aggs agg-by-map]
  (into {}
        (map (fn [[by row]]
               [by (merge-with (fn [[agg-fn _] res]
                                 ;; Invoke the completing function
                                 (agg-fn res))
                               (get agg-by-map (::by row))
                               (merge (core/dissoc row ::by) by))]))
        aggs))

(defn aggregate-by
  "Aggregates a relation by (possibly) multiple groupings.

  Aggregates on multiple aggregation levels in one single pass.

  Takes a map of grouping keys to aggregation map.
  The grouping keys can be either a bare key or a vector of keys.
  An empty vector creates a grouping for all entries.

  The aggregation map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function with identity.

  Example:
  (|> #{{:category :a, :subcategory :x, :val 1}
        {:category :a, :subcategory :x, :val 2}
        {:category :a  :subcategory :y, :val 10}
        {:category :a  :subcategory :y, :val 20}
        {:category :b  :subcategory :Z  :val 100}}
      (aggregate-by {[] {:total [+ :val]}
                     :category {:total [+ :val]}
                     [:category :subcategory] {:total [+ :val]}}))

  => #{{:total 30, :category :a, :subcategory :y}
       {:total 100, :category :b, :subcategory :Z}
       {:total 33, :category :a}
       {:total 3, :category :a, :subcategory :x}
       {:total 133}
       {:total 100, :category :b}}

  This can be expressed using a rollup:
  (aggregate-by (rollup {:total [+ :val]} :category :subcategory))"

  ([agg-by-map]
   (fn [rf]
     (let [aggs (volatile! {})]
       (fn
         ([] (rf))
         ([res]
          (->> (-complete-aggs @aggs agg-by-map)
               vals
               (reduce rf res)
               rf))
         ([res item]
          (vreset!
           aggs
           (reduce-kv
            (-aggs-reducer item)
            @aggs
            agg-by-map))
          res)))))
  ([ks agg]
   (aggregate-by {ks agg}))
  ([ks key agg & more]
   (aggregate-by {ks (apply hash-map key agg more)})))

(defn aggregate-over
  "Aggregates and then joins those aggregates with self.

  See aggregate-by.

  Example:
  (|> #{{:category :a, :subcategory :x, :val 1}
        {:category :a, :subcategory :x, :val 2}
        {:category :a  :subcategory :y, :val 10}
        {:category :a  :subcategory :y, :val 20}
        {:category :b  :subcategory :Z  :val 100}}
      (aggregate-over {[] {:total/sum [+ :val]}
                       :category {:cat/sum [+ :val]}
                       [:category :subcategory] {:subcat/sum [+ :val]}}))

  => #{{:category :a, :subcategory :y, :val 20, :total/sum 133, :cat/sum 33, :subcat/sum 30}
       {:category :a, :subcategory :y, :val 10, :total/sum 133, :cat/sum 33, :subcat/sum 30}
       {:category :a, :subcategory :x, :val 1, :total/sum 133, :cat/sum 33, :subcat/sum 3}
       {:category :a, :subcategory :x, :val 2, :total/sum 133, :cat/sum 33, :subcat/sum 3}
       {:category :b, :subcategory :Z, :val 100, :total/sum 133, :cat/sum 100, :subcat/sum 100}}"
  ([agg-by-map]
   (fn [rf]
     (let [aggs (volatile! {})
           items (volatile! (transient []))
           expanded-ks (map (fn [ks] (if (keyword? ks) [ks] ks)) (keys agg-by-map))]
       (fn
         ([] (rf))
         ([res]
          (let [completed-aggs (-complete-aggs @aggs agg-by-map)]
            (->> @items
                 persistent!
                 (reduce
                  (fn [res row]
                    (->> (reduce
                          (fn [row ks]
                            (merge row (get completed-aggs (select-keys row ks))))
                          row
                          expanded-ks)
                         (rf res)))
                  res)
                 rf)))
         ([res item]
          (vswap! items conj! item)
          (vreset! aggs (reduce-kv
                         (-aggs-reducer item)
                         @aggs
                         agg-by-map))
          res)))))
  ([ks agg]
   (aggregate-over {ks agg}))
  ([ks key agg & more]
   (aggregate-over {ks (apply hash-map key agg more)})))

(defn aggregate
  "Aggregates a relation.

  aggs-map should be a map from key to a vector of agg-fn, key-fn.
  agg-fn must be a reducing function with identity.

  Example:
  (|> #{{:val 1} {:val 2}}
      (aggregate {:val [+ :val]}))
  => #{{:val 3}}"
  ([aggs-map]
   (aggregate-by [] aggs-map))
  ([key agg & more]
   (aggregate-by [] (apply hash-map key agg more))))

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

(defn- normalize
  "Normalizes a relation.
  Returns a map of namespace to distinct maps with keys for only that namespace."
  ([]
   (transient {}))
  ([relmap]
   (-> relmap
       persistent!
       (update-vals persistent!)))
  ([relmap row]
   (let [by-ns
         (-> (reduce-kv
              (fn [m k v]
                (let [nspace (keyword (namespace k))]
                  (assoc! m nspace
                   (assoc! (get m nspace (transient {})) k v))))
              (transient {})
              row)
             (persistent!)
             (update-vals persistent!))]
     (reduce-kv
      (fn [relmap ns row]
        (assoc! relmap
                ns
                (conj! (get relmap ns (transient #{})) row)))
      relmap
      by-ns))))

(defn union
  "Distinct union with rel."
  [rel]
  (comp
   (fn [rf]
     (fn
       ([] (rf))
       ([res] (rf (reduce rf res rel)))
       ([res item] (rf res item))))
   (distinct)))

(defn union-all
  "Union with rel.

  Keeps duplicated rows."
  [rel]
  (fn [rf]
    (fn
      ([] (rf))
      ([res] (rf (reduce rf res rel)))
      ([res item] (rf res item)))))

(defn difference
  "Distinct rows without the rows in rel."
  [rel]
  (comp (remove (set rel))
        (distinct)))

(defn intersection
  "Distinct rows that are also in rel."
  [rel]
  (comp (filter (set rel))
        (distinct)))

(defn simple-stats-agg
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

(defn extend-simple-stats
  "Extends stats generated by simple-stats-agg.

  Example:
  (|> #{{:val 1} {:val 2}}
      (aggregate {:val [simple-stats-agg :val]})
      (extend-stats :val))
  => #{{:min-val 1, :max-val 2, :count 2, :avg-val 3/2, :sum-val 3}}"

  [k]
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

(defn extend-kv [k]
  (let [ns (namespace k)]
    (map #(merge
           (core/dissoc % k)
           (update-keys
            (k %)
            (fn [k']
              (keyword ns (name k'))))))))

(defn expand-kv [k]
  (let [ns (namespace k)]
    (mapcat (fn [row]
              (map (fn [[key val]]
                     (core/assoc (core/dissoc row k)
                                 (keyword ns "key") key
                                 (keyword ns "val") val))
                   (k row))))))

(defn expand-seq [k]
  (mapcat (fn [row]
            (map (fn [val]
                   (core/assoc row k val))
                 (k row)))))

;; Helpers

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

(def count
  "Count aggregation

  Returns the rowcount."
  [+ (constantly 1)])

(defn rollup
  "Helper function to generate agg-by-map needed for aggregate by"
  [agg-map & rollups]
  (->> rollups
       (iterate butlast)
       (take-while seq)
       (cons (list))
       (reduce (fn [agg-by-map grouping]
                 (core/assoc agg-by-map (flatten grouping) agg-map))
               {})))
