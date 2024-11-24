(ns johanwiren.se.relation-test
  (:require [clojure.test :refer [deftest is testing]]
            [johanwiren.se.relation :as r :refer [|>]])
  #?(:cljs (:require-macros johanwiren.se.relation)))

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

(deftest relation-realisation-test
  (testing "Set->relation->set"
    (let [res (-> artist r/relation r/set)]
      (is (identical? artist res) "No transformation passes through untouched")
      (is (set? res))
      (is (= artist res)))
    (let [res (-> artist r/relation (r/update :artist/name identity) r/set)]
      (is (set? res))
      (is (= artist res))))
  (testing "Set->relation->seq"
    (let [res (-> artist r/relation r/seq)]
      (is (seq? res))
      (is (= (seq artist) res)))
    (let [res (-> artist r/relation (r/update :artist/name identity) r/seq)]
      (is (seq? res))
      (is (= (seq artist) res))))
  (testing "Set->relation->vec"
    (let [res (-> artist r/relation r/vec)]
      (is (vector? res))
      (is (= (vec artist) res)))
    (let [res (-> artist r/relation (r/update :artist/name identity) r/vec)]
      (is (vector? res))
      (is (= (vec artist) res)))))

(deftest aggregate-test
  (testing "It aggregates into a relation"
    (is (= #{#:album{:length 4610}}
           (|> song
               (r/aggregate {:album/length [+ :song/length]})))))
  (testing "It completes after aggregating"
    (is (= #{#:album{:length -4610}}
           (|> song
               (r/aggregate {:album/length [(completing + -) :song/length]}))))))

(deftest project-test
  (testing "It projects keys"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> artist
               (r/project [:artist/band-name]))))))

(deftest select-test
  (testing "It selects rows"
    (is (= #{#:artist{:name "Jack White", :band-name "The White Stripes"}
             #:artist{:name "Meg White", :band-name "The White Stripes"}}
           (|> artist
               (r/select (comp #{"The White Stripes"} :artist/band-name)))))
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> artist
               (r/project [:artist/band-name]))))))

(deftest join-test
  (testing "Cross join"
    (is (= (* (count artist) (count song))
           (-> song
               (r/join artist {})
               r/set
               count))))
  (testing "Yrel precedence when merging keys"
    (is (= #{{:b/k 1, :common 2, :a/k 1}}
           (|> #{{:a/k 1 :common 1}}
               (r/join #{{:b/k 1 :common 2}}
                       {:a/k :b/k})))))
  (testing "It joins rows"
    (is (= #{#:artist{:name "Steve Harris"}
             #:artist{:name "Bruce Dickinson"}
             #:artist{:name "Dave Murray"}
             #:artist{:name "Adrian Smith"}
             #:artist{:name "Nicko McBrain"}}
           (|> song
               (r/select (comp #(< 400 %) :song/length))
               (r/join artist {:song/band-name :artist/band-name})
               (r/project [:artist/name])))))
  (testing "It omits unmatched right rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}}
           (|> song
               (r/join artist {:song/band-name :artist/band-name})
               (r/project [:artist/band-name])))))
  (testing "It omits unmatched left rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}}
           (|> artist
               (r/join song {:artist/band-name :song/band-name})
               (r/project [:artist/band-name]))))))

(deftest left-join-test
  (testing "It joins rows"
    (is (= #{{:artist/band-name "Iron Maiden", :song/album-name "The Number of the Beast"}
             #:artist{:band-name "The White Stripes"}
             {:artist/band-name "Iron Maiden", :song/album-name "Iron Maiden"}}
           (|> artist
               (r/left-join song {:artist/band-name :song/band-name})
               (r/project [:artist/band-name :song/album-name])))))
  (testing "It keeps unmatched left rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> artist
               (r/left-join song {:artist/band-name :song/band-name})
               (r/project [:artist/band-name]))))))

(deftest right-join-test
  (testing "It joins rows"
    (is (= #{{:artist/band-name "Iron Maiden",
              :song/album-name "The Number of the Beast"}
             #:artist{:band-name "The White Stripes"}
             {:artist/band-name "Iron Maiden", :song/album-name "Iron Maiden"}}
           (|> song
               (r/right-join artist {:song/band-name :artist/band-name})
               (r/project [:artist/band-name :song/album-name])))))
  (testing "It keeps unmatched right rows"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> song
               (r/right-join artist {:song/band-name :artist/band-name})
               (r/project [:artist/band-name]))))))

(deftest full-join-test
  (testing "It keeps rows on the left"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> artist
               (r/full-join song {:artist/band-name :song/band-name})
               (r/project [:artist/band-name])))))
  (testing "It merges matched rows"
    (is (= #{{:artist/band-name "Iron Maiden", :song/album-name "The Number of the Beast"}
             {:artist/band-name "Iron Maiden", :song/album-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> artist
               (r/full-join song {:artist/band-name :song/band-name})
               (r/project [:artist/band-name :song/album-name])))))
  (testing "It keeps rows on the right"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> song
               (r/full-join artist {:song/band-name :artist/band-name})
               (r/project [:artist/band-name]))))))

(deftest sort-by-test
  (testing "It sorts"
    (let [rel (-> artist
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
             (r/seq rel)))))
  (testing "It uses keeps all entries"
    (let [rel (-> artist
                  (r/sort-by :artist/band-name)
                  (r/project [:artist/name :artist/band-name]))]
      (is (sorted? (r/set rel)))
      (is (= [#:artist{:name "Bruce Dickinson", :band-name "Iron Maiden"}
              #:artist{:name "Adrian Smith", :band-name "Iron Maiden"}
              #:artist{:name "Nicko McBrain", :band-name "Iron Maiden"}
              #:artist{:name "Dave Murray", :band-name "Iron Maiden"}
              #:artist{:name "Steve Harris", :band-name "Iron Maiden"}
              #:artist{:name "Jack White", :band-name "The White Stripes"}
              #:artist{:name "Meg White", :band-name "The White Stripes"}]
             (r/seq rel))))))

(deftest aggregate-by-test
  (testing "It aggregates"
    (is (= #{{:album/length 2351,
              :song/album-name "The Number of the Beast"}
             {:album/length 2259,
              :song/album-name "Iron Maiden"}}
           (|> song
               (r/aggregate-by :song/album-name {:album/length [+ :song/length]}))))))

(deftest stats-test
  (testing "It builds stats"
    (is (= #{#:song{:length-stats
                   {:min 200, :max 428, :count 8, :avg #?(:clj 2351/8 :cljs 293.875), :sum 2351},
                   :album-name "The Number of the Beast"}
            #:song{:length-stats
                   {:min 202, :max 422, :count 8, :avg #?(:clj 2259/8 :cljs 282.375) :sum 2259},
                   :album-name "Iron Maiden"}}
           (|> song
               (r/aggregate-by :song/album-name {:song/length-stats [r/stats-agg :song/length]})))))
  (testing "It extends stats"
    (is (= #{#:song{:album-name "Iron Maiden",
                    :min-length 202,
                    :max-length 422,
                    :count 8,
                    :avg-length #?(:clj 2259/8 :cljs 282.375),
                    :sum-length 2259}
             #:song{:album-name "The Number of the Beast",
                    :min-length 200,
                    :max-length 428,
                    :count 8,
                    :avg-length #?(:clj 2351/8 :cljs 293.875),
                    :sum-length 2351}}
           (|> song
               (r/aggregate-by :song/album-name {:song/length [r/stats-agg :song/length]})
               (r/extend-stats :song/length))))))

(deftest aggregate-over-test
  (testing "It joins aggregates"
    (is (= #{{:song/name "Children of the Damned", :album/length 2351}
             {:song/name "Transylvania", :album/length 2259}
             {:song/name "Charlotte the Harlot", :album/length 2259}
             {:song/name "Prowler", :album/length 2259}
             {:song/name "Iron Maiden", :album/length 2259}
             {:song/name "Run to the Hills", :album/length 2351}
             {:song/name "The Prisoner", :album/length 2351}
             {:song/name "22 Acacia Avenue", :album/length 2351}
             {:song/name "Phantom Of the Opera", :album/length 2259}
             {:song/name "Invaders", :album/length 2351}
             {:song/name "Remember Tomorrow", :album/length 2259}
             {:song/name "Running Free", :album/length 2259}
             {:song/name "Hallowed Be Thy Name", :album/length 2351}
             {:song/name "Gangland", :album/length 2351}
             {:song/name "Strange World", :album/length 2259}
             {:song/name "The Number of the Beast", :album/length 2351}}
           (|> song
               (r/aggregate-over :song/album-name {:album/length [+ :song/length]})
               (r/project [:song/name :album/length]))))))

(deftest aggs-test
  (testing "count-agg"
    (is (= #{{:song/count 16}}
           (|> song
               (r/aggregate {:song/count r/count-agg})))))
  (testing "set-agg"
    (is (= #{#:album{:names #{"The Number of the Beast" "Iron Maiden"}}}
           (|> song
               (r/aggregate {:album/names [r/set-agg :song/album-name]})))))
  (testing "vec-agg"
    (is (= #{#:song{:lengths [200 202 223 226 230 236 249 254 265 274 330 334 343 394 422 428]}}
           (|> song
               (r/sort-by :song/length)
               (r/aggregate {:song/lengths [r/vec-agg :song/length]}))))))

(deftest expand-kv-test
  (testing "It expands maps"
    (is (= #{#:song{:key :max, :val 428}
             #:song{:key :sum, :val 4610}
             #:song{:key :avg, :val #?(:clj 2305/8 :cljs 288.125)}
             #:song{:key :min, :val 200}
             #:song{:key :count, :val 16}}
           (|> song
               (r/aggregate {:song/length [r/stats-agg :song/length]})
               (r/expand-kv :song/length))))))

(deftest extend-kv-test
  (testing "It extends maps"
    (is (= #{#:song{:album-name "Iron Maiden",
                    :min 202,
                    :max 422,
                    :count 8,
                    :avg #?(:clj 2259/8 :cljs 282.375),
                    :sum 2259}
             #:song{:album-name "The Number of the Beast",
                    :min 200,
                    :max 428,
                    :count 8,
                    :avg #?(:clj 2351/8 :cljs 293.875),
                    :sum 2351}}
           (|> song
               (r/aggregate-by :song/album-name {:song/length [r/stats-agg :song/length]})
               (r/extend-kv :song/length))))))

(deftest expand-seq
  (testing "It expands seqs"
    (is (= #{#:album{:names "Iron Maiden"}
             #:album{:names "The Number of the Beast"}}
           (|> song
               (r/aggregate {:album/names [r/set-agg :song/album-name]})
               (r/expand-seq :album/names))))))

(deftest usecase-test
  (testing "Find the songs on each album longer than the average for that album"
    (is (= #{#:song{:name "22 Acacia Avenue"}
             #:song{:name "Strange World"}
             #:song{:name "Remember Tomorrow"}
             #:song{:name "The Prisoner"}
             #:song{:name "Hallowed Be Thy Name"}
             #:song{:name "Phantom Of the Opera"}}
           (|> song
               (r/aggregate-over :song/album-name {:album/song-length [r/stats-agg :song/length]})
               (r/extend-stats :album/song-length)
               (r/extend :song/longer-than-avg?
                 (fn [{:song/keys [length]
                       :album/keys [avg-song-length]}]
                   (< avg-song-length length)))
               (r/select :song/longer-than-avg?)
               (r/project [:song/name]))))))

(deftest normalize
  (testing "It normalizes"
    (is (= {:song song
            :artist artist}
           (-> song
               (r/join artist {})
               r/normalize))))
  (testing "With missing attributes"
    (is (= {:a #{{:a/key :val}}
            :b #{{:b/key :val}}}
           (-> #{{:a/key :val}}
               (r/union #{{:b/key :val}})
               r/normalize)))))


(comment
  (require '[kixi.stats.core :as stats])



  (|> (r/relation [{:a/k 1} {:a/k 1}])
      (r/aggregate :b [+ :a/k]))

  (clojure.set/join #{{:a/k 1 :common 1}
                      {:a/k 2 :common 2}}
                    #{{:b/k 1 :common 2}
                      {:c 1}
                      {:c 2}}
                    {:a/k :b/k})

  (|> (r/relation #{{:a/k 1 :common 1}})
      (r/join (r/relation #{{:b/k 1 :common 2}}) {:a/k :b/k}))

  (= (|> (r/relation artist)
         (r/join song {:artist/band-name :song/band-name}))
     (clojure.set/join artist song {:artist/band-name :song/band-name}))

  (= (|> (r/relation song)
         (r/join artist {:song/band-name :artist/band-name}))
     (clojure.set/join song artist {:song/band-name :artist/band-name}))

  (let [n 10000]
    (print "relation")
    (time
     (dotimes [_ n]
       (|> (r/relation song)
           (r/join (r/relation artist) {:song/band-name :artist/band-name})
           (r/project [:artist/band-name])
           (r/join (r/relation song) {:artist/band-name :song/band-name}))
       nil))

    (print "clojure.set")
    (time
     (dotimes [_ n]
       (-> song
           (clojure.set/join artist {:song/band-name :artist/band-name})
           (clojure.set/project [:artist/band-name])
           (clojure.set/join song {:artist/band-name :song/band-name}))
       nil)))

  (|> (r/relation song)
      (r/join (r/relation artist) {:song/band-name :artist/band-name})
      (r/aggregate-by :song/album-name {:album/length [+ :song/length]
                                        :album/artists [r/set-agg :artist/name]}))

  (|> (r/relation #{{:a 1} {:a 2}})
      (r/assoc :a 2)
      (r/aggregate {:a/sum [+ :a]}))

  nil)
