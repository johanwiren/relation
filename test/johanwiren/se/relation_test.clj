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
                   :length 223}
            #:song{:number 1
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Invaders"
                   :length 200}
            #:song{:number 2
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Children of the Damned"
                   :length 274}
            #:song{:number 3
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "The Prisoner"
                   :length 334}
            #:song{:number 4
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "22 Acacia Avenue"
                   :length 394}
            #:song{:number 5
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "The Number of the Beast"
                   :length 265}
            #:song{:number 6
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Run to the Hills"
                   :length 230}
            #:song{:number 7
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Gangland"
                   :length 226}
            #:song{:number 8
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Hallowed Be Thy Name"
                   :length 428}})

(deftest relation-test
  (testing "Set->relation->set"
    (is (= artist (-> artist r/relation r/set)))))

(deftest aggregate-test
  (testing "It aggregates into a relation"
    (is (= #{#:album{:length 4610, :max-song-length 428}}
           (|> (r/relation song)
               (r/aggregate {:album/length [+ :song/length]
                             :album/max-song-length [max :song/length]}))))))

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
    (is (= #{{:artist/band-name "Iron Maiden", :song/album-name "The Number of the Beast"}
             #:artist{:band-name "The White Stripes"}
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
    (is (= #{#:song{:album-name "Iron Maiden",
                    :avg-length 2259/8,
                    :max-length 422}
             #:song{:album-name "The Number of the Beast",
                    :avg-length 2351/8,
                    :max-length 428}}
           (|> (r/relation song)
               (r/aggregate-by :song/album-name {:song/avg-length [r/avg-agg :song/length]
                                                 :song/max-length [max :song/length]}))))))

(deftest aggregate-over-test
  (testing "It joins aggregates"
    (is (= #{#:song{:album-name "Iron Maiden", :avg-length 2259/8}
             #:song{:album-name "The Number of the Beast", :avg-length 2351/8}}
           (|> (r/relation song)
               (r/aggregate-over :song/album-name {:song/avg-length [r/avg-agg :song/length]})
               (r/project [:song/album-name :song/avg-length]))))))

(deftest usecase-test
  (testing "Find the songs on each album longer than the average for that album"
    (is (= #{#:song{:name "22 Acacia Avenue"}
             #:song{:name "Strange World"}
             #:song{:name "Remember Tomorrow"}
             #:song{:name "The Prisoner"}
             #:song{:name "Hallowed Be Thy Name"}
             #:song{:name "Phantom Of the Opera"}}
           (|> (r/relation song)
               (r/aggregate-over :song/album-name {:song/avg-length [r/avg-agg :song/length]})
               (r/extend :song/longer-than-avg?
                 (fn [{:song/keys [length avg-length]}]
                   (< avg-length length)))
               (r/select :song/longer-than-avg?)
               (r/project [:song/name]))))))

(deftest normalize
  (testing "It normalizes"
    (is (= {:song song
            :artist artist}
           (-> (r/relation song)
               (r/join (r/relation artist) {})
               r/normalize))))
  (testing "With missing attributes"
    (is (= {:a #{{:a/key :val}}
            :b #{{:b/key :val}}}
           (-> (r/relation #{{:a/key :val}})
               (r/union (r/relation #{{:b/key :val}}))
               r/normalize)))))
