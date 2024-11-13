(ns johanwiren.se.relation.impl
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [set seq count]))

(defprotocol RelationP
  (set [rel])
  (compose [rel xform])
  (entries [rel])
  (count [rel]))

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

  (count [_]
    (core/count rel))

  (entries [_]
    (sequence xform rel)))
