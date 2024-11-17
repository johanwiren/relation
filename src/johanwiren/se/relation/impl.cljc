(ns johanwiren.se.relation.impl
  #?(:clj (:require [clojure.core :as core])
     :cljs (:require [cljs.core :as core]))
  (:refer-clojure :exclude [keys set seq count vec]))

(deftype Relation [xform rel])

(defn vec [rel]
  (into []
        (comp (.-xform rel)
              (distinct))
        (.-rel rel)))

(defn seq [rel]
  (sequence (comp (.-xform rel)
                  (distinct))
            (.-rel rel)))

(defn set [rel]
  (into (empty (.-rel rel))
        (.-xform rel)
        (.-rel rel)))

(defn compose [rel xform]
  (->Relation (comp (.-xform rel) xform) (.-rel rel)))

(defn entries [rel]
  (sequence (.-xform rel) (.-rel rel)))

(defn keys [rel]
  (core/keys (first (.-rel rel))))

(defn relation? [x]
  (instance? Relation x))
