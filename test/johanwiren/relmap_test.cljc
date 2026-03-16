(ns johanwiren.relmap-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [johanwiren.relation-test :as rel-test]
   [johanwiren.relmap :as sut]
   [johanwiren.relation :as r]
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])))


(def relmap
  {:artist rel-test/artist})


(def pk-map
  {:artist #{:artist/name}})


(deftest diff-test
  (testing "added"
    (let [addition #{{:artist/name "Belinda Carlisle" :artist/band-name "The Go Go's"}}]
      (is (= {:artist {:added addition
                       :removed #{}
                       :updated #{}}}
             (sut/diff
               pk-map
               relmap
               (update relmap :artist r/|> (r/union addition)))))))

  (testing "removed"
    (let [removal #{{:artist/name "Nicko McBrain" :artist/band-name "Iron Maiden"}}]
      (is (= {:artist {:removed removal
                       :added #{}
                       :updated #{}}}
             (sut/diff
               pk-map
               relmap
               (update relmap :artist r/|> (remove removal)))))))

  (testing "updated"
    (is (= {:artist
            {:updated
             #{#:artist{:name "Meg White", :band-name "THE WHITE STRIPES"}
               #:artist{:name "Bruce Dickinson", :band-name "IRON MAIDEN"}
               #:artist{:name "Jack White", :band-name "THE WHITE STRIPES"}
               #:artist{:name "Dave Murray", :band-name "IRON MAIDEN"}
               #:artist{:name "Steve Harris", :band-name "IRON MAIDEN"}
               #:artist{:name "Nicko McBrain", :band-name "IRON MAIDEN"}
               #:artist{:name "Adrian Smith", :band-name "IRON MAIDEN"}}
             :added #{}
             :removed #{}}}
           (sut/diff
             pk-map
             relmap
             (update relmap :artist r/|> (r/update :artist/band-name str/upper-case)))))))
