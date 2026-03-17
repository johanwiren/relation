(ns johanwiren.relmap
  (:require [johanwiren.relation :as rel]))

(defn- -join [pk-map kind rm-a rm-b]
  (reduce (fn [relmap [relvar rel]]
            (update relmap
                    relvar
                    rel/|>
                    (rel/join rel
                              (into {}
                                    (fn [ks] {ks ks})
                                    (get pk-map relvar))
                              kind)))
          rm-a
          rm-b))

(defn join
  [pk-map rm-a rm-b]
  (-join pk-map :inner rm-a rm-b))

(defn left-join
  [pk-map rm-a rm-b]
  (-join pk-map :outer rm-a rm-b))

(defn right-join
  [pk-map rm-a rm-b]
  (-join pk-map :right rm-a rm-b))

(defn full-join
  [pk-map rm-a rm-b]
  (-join pk-map :full rm-a rm-b))

(defn row-diff [row-a row-b ks]
  (let [attrs (into (set (keys row-a))
                    (keys row-b))]
    (into {}
          (keep (fn [attr]
                  (when (or (get ks attr)
                            (not= (get row-a attr)
                                  (get row-b attr)))
                    [attr (get row-b attr)])))
          attrs)))

(defn rel-diff [rel-a rel-b ks]
  (let [a-idx (rel/index rel-a ks)
        b-idx (rel/index rel-b ks)
        idx-ks (into (set (keys a-idx))
                     (keys b-idx))]
    (->
     (reduce (fn [diff idx-k]
               (let [a-row (first (get a-idx idx-k))
                     b-row (first (get b-idx idx-k))
                     [kind row] (cond
                                  (and a-row
                                       b-row
                                       (not= a-row b-row))
                                  [:updated (row-diff a-row b-row ks)]

                                  (and (not a-row)
                                       b-row)
                                  [:added b-row]

                                  (and (not b-row)
                                       a-row)
                                  [:removed a-row]

                                  :else nil)]
                 (if kind
                   (assoc! diff kind (conj! (get diff kind) row))
                   diff)))
             (transient
              {:added (transient #{})
               :removed (transient #{})
               :updated (transient #{})})
             idx-ks)
     (persistent!)
     (update-vals persistent!))))

(defn diff [rm-a rm-b pk-map]
  (let [relvars (into (set (keys rm-a))
                      (keys rm-b))]
    (reduce (fn [diff relvar]
              (let [rel-diff (rel-diff (get rm-a relvar)
                                       (get rm-b relvar)
                                       (get pk-map relvar))]
                (assoc diff relvar rel-diff)))
            {}
            relvars)))

(defn normalize [relmap]
  (rel/|>normalized (apply concat (vals relmap))))
