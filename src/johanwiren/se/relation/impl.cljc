(ns johanwiren.se.relation.impl
  (:refer-clojure :exclude [set seq]))

(defprotocol RelationP
  (set [rel])
  (compose [rel xform])
  (entries [rel]))

(deftype Relation [xform rel]
  #?(:clj clojure.lang.Seqable
     :cljs ISeqable)
  (seq [_]
    (sequence (comp xform (distinct)) rel))

  RelationP
  (set [_]
    (into (empty rel) xform rel))

  (compose [_ xform']
    (Relation. (comp xform xform') rel))

  (entries [_]
    (sequence xform rel)))
