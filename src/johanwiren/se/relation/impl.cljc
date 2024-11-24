(ns johanwiren.se.relation.impl
  #?(:clj (:require [clojure.core :as core])
     :cljs (:require [cljs.core :as core]))
  (:refer-clojure :exclude [keys set seq count vec counted?]))

(deftype Relation [xform rel])

(defn vec [rel]
  (if (= identity (.-xform rel))
    (core/vec (.-rel rel))
    (into []
          (comp (.-xform rel)
                (distinct))
          (.-rel rel))))

(defn seq [rel]
  (if (= identity (.-xform rel))
    (core/seq (.-rel rel))
    (sequence (comp (.-xform rel)
                    (distinct))
              (.-rel rel))))

(defn set [rel]
  (if (= identity (.-xform rel))
    (.-rel rel)
    (into (empty (.-rel rel))
          (.-xform rel)
          (.-rel rel))))

(defn entries [rel]
  (if (= identity (.-xform rel))
    (core/seq (.-rel rel))
    (sequence (.-xform rel) (.-rel rel))))

(defn compose [rel xform]
  (->Relation (comp (.-xform rel) xform) (.-rel rel)))

(defn relation? [x]
  (instance? Relation x))

(defn counted? [rel]
  (= identity (.-xform rel)))

(defn count [rel]
  (and (counted? rel)
       (core/count (.-rel rel))))
