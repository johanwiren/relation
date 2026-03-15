(ns johanwiren.relation-test
  (:require [clojure.test :refer [deftest is testing]]
            [johanwiren.relation :as r :refer [|>set |>seq |>vec |> |>first |>last]])
  #?(:cljs (:require-macros johanwiren.relation)))

(def genre #{{:genre/id 0
              :genre/name "New wave of British heavy metal"
              :genre/parent-id 1}
             {:genre/id 1
              :genre/name "Metal"
              :genre/parent-id 2}
             {:genre/id 2
              :genre/name "Popular"
              :genre/parent-id nil}})

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
                   :genre "New wave of British heavy metal"
                   :length 236}
            #:song{:number 2
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Remember Tomorrow"
                   :genre "New wave of British heavy metal"
                   :length 330}
            #:song{:number 3
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Running Free"
                   :genre "New wave of British heavy metal"
                   :length 202}
            #:song {:number 4
                    :band-name "Iron Maiden"
                    :album-name "Iron Maiden"
                    :name "Phantom Of the Opera"
                    :genre "New wave of British heavy metal"
                    :length 422}
            #:song{:number 5
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Transylvania"
                   :genre "New wave of British heavy metal"
                   :length 249}
            #:song{:number 6
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Strange World"
                   :genre "New wave of British heavy metal"
                   :length 343}
            #:song{:number 7
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Charlotte the Harlot"
                   :genre "New wave of British heavy metal"
                   :length 254}
            #:song{:number 8
                   :band-name "Iron Maiden"
                   :album-name "Iron Maiden"
                   :name "Iron Maiden"
                   :genre "New wave of British heavy metal"
                   :length 223}
            #:song{:number 1
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Invaders"
                   :genre "New wave of British heavy metal"
                   :length 200}
            #:song{:number 2
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Children of the Damned"
                   :genre "New wave of British heavy metal"
                   :length 274}
            #:song{:number 3
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "The Prisoner"
                   :genre "New wave of British heavy metal"
                   :length 334}
            #:song{:number 4
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "22 Acacia Avenue"
                   :genre "New wave of British heavy metal"
                   :length 394}
            #:song{:number 5
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "The Number of the Beast"
                   :genre "New wave of British heavy metal"
                   :length 265}
            #:song{:number 6
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Run to the Hills"
                   :genre "New wave of British heavy metal"
                   :length 230}
            #:song{:number 7
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Gangland"
                   :genre "New wave of British heavy metal"
                   :length 226}
            #:song{:number 8
                   :band-name "Iron Maiden"
                   :album-name "The Number of the Beast"
                   :name "Hallowed Be Thy Name"
                   :genre "New wave of British heavy metal"
                   :length 428}})

(def apple-sales
  #{{:category "iphone" :model "iPhone Eclipse" :price 899}
    {:category "iphone" :model "iPhone Eclipse" :price 999}
    {:category "iphone" :model "iPhone Pulse" :price 999}
    {:category "iphone" :model "iPhone Pulse" :price 1099}
    {:category "mac" :model "Mac Mini Turbo" :price 799}
    {:category "mac" :model "Mac Mini Turbo" :price 899}
    {:category "mac" :model "MacBook Airflow" :price 1299}
    {:category "mac" :model "MacBook Airflow" :price 1999}
    {:category "mac" :model "MacBook Airflow" :price 1499}})

(deftest |>fns-test
  (is (set? (|> #{:a})))
  (is (vector? (|> [:a])))
  (is (seq? (|> (seq [:a]))))
  (is (set? (|>set [:a])))
  (is (vector? (|>vec #{:a})))
  (is (seq? (|>seq [:a])))
  (is (nil? (|>first [])))
  (is (= 1 (|>first (range)
                    (r/select odd?))))
  (is (nil? (|>last [])))
  (is (= 9 (|>last (range)
                   (take 10)
                   (r/select odd?))))
  (is (= [1 2]
         (-> (range 10)
             (|> (take 2))
             (|> (map inc))))))

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
               (r/project [:artist/band-name])))))
  (testing "Distinct"
    (is (= #{{:count 1}}
           (|> song
               (r/project [:song/band-name])
               (distinct)
               (r/aggregate :count r/count)))))
  (testing "SQL-like projection"
    (is (= #{{:count 16}}
           (|> song
               (r/project [:song/band-name])
               (r/aggregate :count r/count))))))

(deftest select-test
  (testing "It selects rows"
    (is (= #{#:artist{:name "Jack White", :band-name "The White Stripes"}
             #:artist{:name "Meg White", :band-name "The White Stripes"}}
           (|> artist
             (r/select (comp #{"The White Stripes"} :artist/band-name)))))))

(deftest join-test
  (testing "Cross join"
    (is (= (* (count artist) (count song))
           (count
            (|> song
              (r/join artist {}))))))
  (testing "Empty xrel"
    (is (= #{}
           (|> #{}
             (r/join song {})))))
  (testing "Empty yrel"
    (is (= #{}
           (|> song
             (r/join #{} {})))))
  (testing "Eduction"
    (is (= #{{:song/name "22 Acacia Avenue", :artist/band-name "Iron Maiden"}}
           (|> artist
               (r/join (r/|>eduction song
                                     (r/sort-by :song/name)
                                     (take 1))
                       {:artist/band-name :song/band-name})
               (r/project [:song/name :artist/band-name])))))
  (testing "Natural join"
    (is (= #{#:artist{:name "Steve Harris"}
             #:artist{:name "Bruce Dickinson"}
             #:artist{:name "Dave Murray"}
             #:artist{:name "Adrian Smith"}
             #:artist{:name "Nicko McBrain"}}
           (|> song
               (r/rename {:song/band-name :artist/band-name})
               (r/join artist)
               (r/project [:artist/name])))))
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
             (r/project [:artist/band-name])))))
  (testing "It self joins"
    (is (= #{{:genre/name "Metal", :parent/name "Popular"}
             {:genre/name "New wave of British heavy metal", :parent/name "Metal"}}
           (|> genre
             (r/join :self/parent {:parent/id :genre/parent-id})
             (r/project [:genre/name :parent/name]))))))

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
             (r/project [:artist/band-name])))))
  (testing "It keeps left rows when joining an empty relation"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> artist
             (r/left-join #{} {})
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
             (r/project [:artist/band-name])))))
  (testing "It keeps right rows when joining an empty relation"
    (is (= #{#:artist{:band-name "Iron Maiden"}
             #:artist{:band-name "The White Stripes"}}
           (|> #{}
             (r/right-join artist {})
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

(deftest anti-join
  (testing "It keeps entries not in right"
    (is (= #{#:artist{:name "Jack White", :band-name "The White Stripes"}
             #:artist{:name "Meg White", :band-name "The White Stripes"}}
           (|> artist
             (r/anti-join song {:artist/band-name :song/band-name}))))))

(deftest sort-by-test
  (testing "It sorts"
    (is (= [#:artist{:name "Adrian Smith"}
            #:artist{:name "Bruce Dickinson"}
            #:artist{:name "Dave Murray"}
            #:artist{:name "Jack White"}
            #:artist{:name "Meg White"}
            #:artist{:name "Nicko McBrain"}
            #:artist{:name "Steve Harris"}]
           (|>vec artist
                  (r/sort-by :artist/name)
                  (r/project [:artist/name])))))
  (testing "It sorts with comparator"
    (is (= [#:artist{:name "Steve Harris"}
            #:artist{:name "Nicko McBrain"}
            #:artist{:name "Meg White"}
            #:artist{:name "Jack White"}
            #:artist{:name "Dave Murray"}
            #:artist{:name "Bruce Dickinson"}
            #:artist{:name "Adrian Smith"}]
           (|>vec artist
                  (r/sort-by :artist/name (fn [a b] (compare b a)))
                  (r/project [:artist/name]))))))

(defn avg
  ([] {:count 0 :sum 0})
  ([{:keys [count sum]}] (if (pos? count)
                           (/ sum count)
                           0))
  ([state val]
   (merge-with + state {:count 1 :sum val})))

(deftest aggregate-by-test
  (testing "It aggregates"
    (is (= #{{:album/length 2351,
              :song/album-name "The Number of the Beast"}
             {:album/length 2259,
              :song/album-name "Iron Maiden"}}
           (|> song
             (r/aggregate-by {:song/album-name {:album/length [+ :song/length]}})))))
  (testing "It aggregates on multiple levels"
    (is (= #{{:avg-price #?(:clj 3497/3 :cljs 1165.6666666666667)}
             {:avg-price 999 :category "iphone"}
             {:avg-price 949 :category "iphone" :model "iPhone Eclipse"}
             {:avg-price 1049 :category "iphone" :model "iPhone Pulse"}
             {:avg-price 1299 :category "mac"}
             {:avg-price 849 :category "mac" :model "Mac Mini Turbo"}
             {:avg-price 1599 :category "mac" :model "MacBook Airflow"}}
           (|>
            apple-sales
            (r/aggregate-by {[] {:avg-price [avg :price]}
                             :category {:avg-price [avg :price]}
                             [:category :model] {:avg-price [avg :price]}}))
           (|>
            apple-sales
            (r/aggregate-by (r/rollup {:avg-price [avg :price]}
                                      :category :model))))))
  (testing "Joining aggs"
    (is (= #{{:category "iphone"
              :model "iPhone Eclipse"
              :price 999
              :category/avg-price 999
              :model/avg-price 949
              :global/avg-price #?(:clj 3497/3 :cljs 1165.6666666666667)}
             {:category "iphone"
              :model "iPhone Eclipse"
              :price 899
              :category/avg-price 999
              :model/avg-price 949
              :global/avg-price #?(:clj 3497/3 :cljs 1165.6666666666667)}}
           (let [aggs
                 (|>
                  apple-sales
                  (r/aggregate-by {[] {:global/avg-price [avg :price]}
                                   :category {:category/avg-price [avg :price]}
                                   [:category :model] {:model/avg-price [avg :price]}}))]
             (|> apple-sales
                 (r/join (|> aggs (r/select (comp nil? :model)))
                         {:category :category})
                 (r/join (|> aggs (r/select :model))
                         {:category :category, :model :model})
                 (r/join (|> aggs (r/select (complement (some-fn :model :category))))
                         {})
                 (r/select (comp #{"iPhone Eclipse"} :model))))))))

(deftest stats-test
  (testing "It builds stats"
    (is (= #{#:song{:length-stats
                    {:min 200, :max 428, :count 8, :avg #?(:clj 2351/8 :cljs 293.875), :sum 2351},
                    :album-name "The Number of the Beast"}
             #:song{:length-stats
                    {:min 202, :max 422, :count 8, :avg #?(:clj 2259/8 :cljs 282.375) :sum 2259},
                    :album-name "Iron Maiden"}}
           (|> song
             (r/aggregate-by :song/album-name {:song/length-stats [r/simple-stats-agg :song/length]})))))
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
             (r/aggregate-by :song/album-name {:song/length [r/simple-stats-agg :song/length]})
             (r/extend-simple-stats :song/length))))))

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
             (r/project [:song/name :album/length])))))
  (testing "It aggregates on multiple levels"
    (is (= #{{:global/avg-price #?(:clj 3497/3 :cljs 1165.6666666666667)
              :category/avg-price 999
              :model/avg-price 949}}
           (|>
            apple-sales
            (r/aggregate-over {[] {:global/avg-price [avg :price]}
                               :category {:category/avg-price [avg :price]}
                               [:category :model] {:model/avg-price [avg :price]}})
            (r/select (comp #{"iPhone Eclipse"} :model))
            (r/project-ns [:global :model :category]))))))

(deftest aggs-test
  (testing "count"
    (is (= #{{:song/count 16}}
           (|> song
             (r/aggregate {:song/count r/count})))))
  (testing "set-agg"
    (is (= #{#:album{:names #{"The Number of the Beast" "Iron Maiden"}}}
           (|> song
             (r/aggregate {:album/names [r/set-agg :song/album-name]})))))
  (testing "vec-agg"
    (is (= #{#:song{:lengths [200 202 223 226 230 236 249 254 265 274 330 334 343 394 422 428]}}
           (|> song
             (r/sort-by :song/length)
             (r/aggregate {:song/lengths [r/vec-agg :song/length]})))))
  (testing "combined agg"
    (is (= #{{:album/names #{"The Number of the Beast" "Iron Maiden"}
              :song/count 16}}
           (|> song
             (r/aggregate {:album/names [r/set-agg :song/album-name]
                           :song/count r/count}))))))

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
             (r/aggregate-by :song/album-name {:song/length [r/simple-stats-agg :song/length]})
             (r/extend-kv :song/length))))))

(deftest expand-seq
  (testing "It expands seqs"
    (is (= #{#:album{:names "Iron Maiden"}
             #:album{:names "The Number of the Beast"}}
           (|> song
             (r/aggregate {:album/names [r/set-agg :song/album-name]})
             (r/expand-seq :album/names))))))

(deftest qualify
  (testing "It qualifies keys"
    (is (= [{:qualify/a 1, :qualify/b 1, :qualify.pre-qualified/c 1}]
           (|> [{:a 1 :b 1 :pre-qualified/c 1}]
               (r/qualify :qualify))))))

(deftest project-pred
  (testing "It projects selected keys"
    (is (= #{#:artist{:name "Steve Harris", :band-name "Iron Maiden"}
             #:artist{:name "Dave Murray", :band-name "Iron Maiden"}
             #:artist{:name "Bruce Dickinson", :band-name "Iron Maiden"}
             #:artist{:name "Nicko McBrain", :band-name "Iron Maiden"}
             #:artist{:name "Adrian Smith", :band-name "Iron Maiden"}}
           (|> song
             (r/join artist {:song/band-name :artist/band-name})
             (r/project-pred (comp #{"artist"} namespace)))))))

(deftest project-ns
  (testing "It projects selected namespaces"
    (is (= #{#:artist{:name "Steve Harris", :band-name "Iron Maiden"}
             #:artist{:name "Dave Murray", :band-name "Iron Maiden"}
             #:artist{:name "Bruce Dickinson", :band-name "Iron Maiden"}
             #:artist{:name "Nicko McBrain", :band-name "Iron Maiden"}
             #:artist{:name "Adrian Smith", :band-name "Iron Maiden"}}
           (|> song
             (r/join artist {:song/band-name :artist/band-name})
             (r/project-ns ["artist"]))))))

(deftest usecase-test
  (testing "Find the songs on each album longer than the average for that album"
    (is (= #{#:song{:name "22 Acacia Avenue"}
             #:song{:name "Strange World"}
             #:song{:name "Remember Tomorrow"}
             #:song{:name "The Prisoner"}
             #:song{:name "Hallowed Be Thy Name"}
             #:song{:name "Phantom Of the Opera"}}
           (|> song
             (r/aggregate-over :song/album-name {:album/song-length [r/simple-stats-agg :song/length]})
             (r/extend-simple-stats :album/song-length)
             (r/extend :song/longer-than-avg?
               (fn [{:song/keys [length]
                     :album/keys [avg-song-length]}]
                 (< avg-song-length length)))
             (r/select :song/longer-than-avg?)
             (r/project [:song/name]))))))

(deftest update-test
  (testing "It updates values"
    (is (= #{#:song{:name "Iron Maiden", :length 43}
             #:song{:name "Prowler", :length 56}
             #:song{:name "The Prisoner", :length 34}
             #:song{:name "Strange World", :length 43}
             #:song{:name "Gangland", :length 46}
             #:song{:name "Running Free", :length 22}
             #:song{:name "Hallowed Be Thy Name", :length 8}
             #:song{:name "Run to the Hills", :length 50}
             #:song{:name "Charlotte the Harlot", :length 14}
             #:song{:name "22 Acacia Avenue", :length 34}
             #:song{:name "The Number of the Beast", :length 25}
             #:song{:name "Transylvania", :length 9}
             #:song{:name "Remember Tomorrow", :length 30}
             #:song{:name "Invaders", :length 20}
             #:song{:name "Phantom Of the Opera", :length 2}
             #:song{:name "Children of the Damned", :length 34}}
           (|> song
               (r/update :song/length mod 60)
               (r/project [:song/name :song/length]))))))


(deftest normalize-test
  (testing "It normalizes"
    (is (= {:song song
            :artist artist}
           (r/|>normalized
               song
             (r/join artist {}))))))

(deftest |>-test
  (testing "It composes clojure.core transducers"
    (is (= #{200}
           (|> song
             (r/sort-by :song/length)
             (map :song/length)
             (take 1)))))
  (testing "It is a normal function, useable in regular update etc"
    (is (= {:artist #{#:artist{:name "Jack White"} #:artist{:name "Meg White"}}
            :song #:song{:name "22 Acacia Avenue"}}
           (-> (r/|>normalized song
                               (r/join artist {}))
               (update :song
                       |>first
                       (r/sort-by :song/name)
                       (r/project [:song/name]))
               (update :artist
                       |>
                       (r/select (comp #{"The White Stripes"} :artist/band-name))
                       (r/project [:artist/name])))))))

(deftest rollup-test
  (testing "It generates rollup groupings"
    (is (= {[] {} [:a :b :c :d] {} [:a :b :c] {} [:a] {}}
           (r/rollup {} :a [:b :c] :d)))))

(deftest as-test
  (testing "It renames things"
    (is (= #{#:tune{:band-name "Iron Maiden"}}
           (|> song
               (r/as :tune)
               (r/project [:tune/band-name]))))))

(deftest update-keys-test
  (testing "It updates keys"
    (is (= #{#{"album-name" "band-name" "name" "genre" "number" "length"}}
           (|>set song
                  (take 1)
                  (r/update-keys (comp str name))
                  (map (comp set keys)))))))

(deftest update-vals-test
  (testing "It updates vals"
    (is (= #{#{"mac" "799"}}
           (|>set apple-sales
                  (r/sort-by :price)
                  (take 1)
                  (r/project [:category :price])
                  (r/update-vals str)
                  (map (comp set vals)))))))

(deftest when|>-test
  (testing "It optionally applies xforms"
    (is (= #{#:song{:name "Run to the Hills"}
             #:song{:name "Gangland"}
             #:song{:name "Invaders"}
             #:song{:name "22 Acacia Avenue"}
             #:song{:name "Transylvania"}
             #:song{:name "Children of the Damned"}
             #:song{:name "Iron Maiden"}
             #:song{:name "Strange World"}
             #:song{:name "Prowler"}
             #:song{:name "Remember Tomorrow"}
             #:song{:name "The Prisoner"}
             {:album/has-long-song? true,
              :song/long? true,
              :song/name "Phantom Of the Opera"}
             #:song{:name "Running Free"}
             {:album/has-long-song? true,
              :song/long? true,
              :song/name "Hallowed Be Thy Name",
              :song/super-long? true}
             #:song{:name "Charlotte the Harlot"}
             #:song{:name "The Number of the Beast"}}
           (|> song
               (r/when|> (fn [{:song/keys [length]}]
                           (< 400 length))
                         (r/assoc :song/long? true)
                         (r/assoc :album/has-long-song? true)
                         (r/when|> (fn [{:song/keys [length]}]
                                     (< 425 length))
                                   (r/assoc :song/super-long? true)))
               (r/project [:song/name :song/long? :song/super-long? :album/has-long-song?]))))))
