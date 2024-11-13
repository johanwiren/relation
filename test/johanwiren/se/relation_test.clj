(ns johanwiren.se.relation-test
  (:require [clojure.test :refer [deftest is testing]]
            [johanwiren.se.relation :as r :refer [|>]]))

(def artist #{{:artist/name "Bruce Dickinson" :artist/band-name "Iron Maiden"}
              {:artist/name "Nicko McBrain" :artist/band-name "Iron Maiden"}
              {:artist/name "Steve Harris" :artist/band-name "Iron Maiden"}
              {:artist/name "Dave Murray" :artist/band-name "Iron Maiden"}
              {:artist/name "Adrian Smith" :artist/band-name "Iron Maiden"}
              {:artist/name "Jack White" :artist/band-name "The White Stripes"}
              {:artist/name "Meg White" :artist/band-name "The White Stripes"}})

(def song #{#:song{:number 1
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Prowler"
                   :length 236}
            #:song{:number 2
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Remember Tomorrow"
                   :length 330}
            #:song{:number 3
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Running Free"
                   :length 202}
            #:song {:number 4
                    :band-name "Iron Maiden"
                    :album-name "Iron Maiden"
                    :name "Phantom Of the Opera"
                    :length 422}
            #:song{:number 5
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Transylvania"
                   :length 249}
            #:song{:number 6
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Strange World"
                   :length 343}
            #:song{:number 7
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Charlotte the Harlot"
                   :length 254}
            #:song{:number 8
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Iron Maiden"
                   :length 223}})

(deftest relation-test
  (testing "Set->relation->set"
    (is (= artist (-> artist r/relation r/set)))))

(deftest aggregate-test
  (testing "It aggregates into a relation"
    (is (= #{{:album/length 2259}}
           (|> (r/relation song)
               (r/aggregate {:album/length [+ :song/length]}))))))

(deftest project-test
  (testing "It projects keys"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> (r/relation artist)
               (r/project [:artist/band-name]))))))

(deftest select-test
  (testing "It selects rows"
    (is (= #{#:artist{:name "Jack White", :band-name "The White Stripes"}
             #:artist{:name "Meg White", :band-name "The White Stripes"}}
           (|> (r/relation artist)
               (r/select (comp #{"The White Stripes"} :artist/band-name)))))
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> (r/relation artist)
               (r/project [:artist/band-name]))))))

(deftest join-test
  (testing "Cross join"
    (is (= (* (count artist) (count song))
           (-> (r/relation song)
               (r/join (r/relation artist) {})
               r/set
               count))))
  (testing "It joins rows"
    (is (= #{#:artist{:name "Steve Harris"}
             #:artist{:name "Bruce Dickinson"}
             #:artist{:name "Dave Murray"}
             #:artist{:name "Adrian Smith"}
             #:artist{:name "Nicko McBrain"}}
           (|> (r/relation song)
               (r/select (comp #(< 400 %) :song/length))
               (r/join (r/relation artist) {:song/band-name :artist/band-name})
               (r/project [:artist/name])))))
  (testing "It omits unmatched right rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}}
           (|> (r/relation song)
               (r/join (r/relation artist) {:song/band-name :artist/band-name})
               (r/project [:artist/band-name])))))
  (testing "It omits unmatched left rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}}
           (|> (r/relation artist)
               (r/join (r/relation song) {:artist/band-name :song/band-name})
               (r/project [:artist/band-name]))))))

(deftest left-join-test
  (testing "It joins rows"
    (is (= #{#:artist{:band-name "The White Stripes"}         
            {:artist/band-name "Iron Maiden", :song/album-name "Iron Maiden"}}
           (|> (r/relation artist)
               (r/left-join (r/relation song) {:artist/band-name :song/band-name})
               (r/project [:artist/band-name :song/album-name])))))
  (testing "It keeps unmatched left rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> (r/relation artist)
               (r/left-join (r/relation song) {:artist/band-name :song/band-name})
               (r/project [:artist/band-name]))))))

(deftest sort-by-test
  (testing "It sorts"
    (let [rel (-> (r/relation artist)
                  (r/sort-by :artist/name)
                  (r/project [:artist/name]))]
      (is (sorted? (r/set rel)))
      (is (= ["Adrian Smith" "Bruce Dickinson" "Dave Murray" "Jack White"
              "Meg White" "Nicko McBrain" "Steve Harris"]
             (map :artist/name (r/seq rel))))
      (is (= [#:artist{:name "Adrian Smith"}
              #:artist{:name "Bruce Dickinson"}
              #:artist{:name "Dave Murray"}
              #:artist{:name "Jack White"}
              #:artist{:name "Meg White"}
              #:artist{:name "Nicko McBrain"}
              #:artist{:name "Steve Harris"}]
             (r/seq rel))))))

(deftest aggregate-by-test
  (testing "It aggregates"
    (is (= #{#:song{:album-name "Iron Maiden", :avg-length 2259/8}}
           (|> (r/relation song)
               (r/aggregate-by :song/album-name {:song/avg-length [r/avg-agg :song/length]}))))))
