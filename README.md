# Relation

Simple relational algebra expressions for Clojure and ClojureScript.

Builds entirely on transducers and can be extended as such.

## Status

Experimental, use at your own risk. Things will probably change.

## The |> function

The `|>` function is the preferred method to interact with this library, it gives a nice way to write relational algebra expressions in linear fashion like this:

```clojure
(|> relation-a
    (r/select (comp pos? :a/val))
    (r/join relation-b {:a/b-id :b/id})
    (r/aggregate :a/sum [+ :a/val]))
```

Since it is a function it can be used anywhere a function is needed:

```clojure
(->
 {:person [{:person/name "Alice" :person/yob 1972}
           {:person/name "Bob" :person/yob 1967}]}
 (update :person
         |>
         (r/select (comp (partial < 1970) :person/yob))
         (r/project [:person/name])))
```

### Why a new way "pipe" to pipe things? Why not just `->` or `->>`?

First of all they are not functions and cannot be used where a function is needed. Second, the `|>` is now used in many places to express pipes of relational algebra expressions.

### I still want to do more processing after the `|>` terminates, like extracting a single value, how do I do that?

Most things should be expressable together with transducers in *clojure.core* and you can also use `|>first` in combination like this:

```clojure
(|>first
 [{:age 42} {:age 19}]
 (r/select (comp (partial < 20) :age))
 ;; Note that we can add normal transducers from clojure.core
 (map :age))

=> 42

```

## Performance

There is no optimizer, so ensuring optimal performance is up to the caller.

## Uniqueness

Similarly to SQL, Relation will not guarantee uniqueness, except `union`, `intersection`, `difference` and `|>set`. It is up to the caller to insert `(distinct)` operations where needed for uniqueness.

## Usage

``` clojure
(require '[johanwiren.relation :as r :refer [|> |>first]])

(def employee #{{:name "Harry" :emp-id 3415 :dept-name "Finance"}
                {:name "Sally" :emp-id 2241 :dept-name "Sales"}
                {:name "George" :emp-id 3401 :dept-name "Finance"}
                {:name "Harriet" :emp-id 2202 :dept-name "Sales"}
                {:name "Mary" :emp-id 1257 :dept-name "Human Resources"}})
                
(def dept #{{:name "Finance" :manager "George"}
            {:name "Sales" :manager "Harriet"}
            {:name "Production" :manager "Charles"}})

```

Joining

``` clojure

(|> employee
    (r/join dept {:dept-name :name}))

=> ({:name "Finance", :emp-id 3415, :dept-name "Finance", :manager "George"}
    {:name "Sales", :emp-id 2241, :dept-name "Sales", :manager "Harriet"}
    {:name "Sales", :emp-id 2202, :dept-name "Sales", :manager "Harriet"}
    {:name "Finance", :emp-id 3401, :dept-name "Finance", :manager "George"})

```

### Operations

Relation provides a rich set of operations

* select
* assoc
* dissoc
* rename
* extend
* update
* project
* join (including self-join)
* left-join
* right-join
* full-join
* recursive-join
* anti-join
* aggregate-by
* aggregate-over
* aggregate
* sort-by
* union
* union-all
* difference
* intersection
* extend-kv
* expand-kv
* expand-seq

And they compose easily like this:

``` clojure
(|> employee
    (r/join dept {:dept-name :name})
    (r/aggregate-over :dept-name {:dept-colleagues [r/vec-agg :name]})
    (r/update :dept-colleagues count)
    (r/sort-by :emp-id))

=> #{{:name "Finance",
      :emp-id 3415,
      :dept-name "Finance",
      :manager "George",
      :dept-colleagues 2}
     {:name "Sales",
      :emp-id 2202,
      :dept-name "Sales",
      :manager "Harriet",
      :dept-colleagues 2}
     {:name "Finance",
      :emp-id 3401,
      :dept-name "Finance",
      :manager "George",
      :dept-colleagues 2}
     {:name "Sales",
      :emp-id 2241,
      :dept-name "Sales",
      :manager "Harriet",
      :dept-colleagues 2}}

```

They're just transducers though so they fit into any transducing process. There is no need to use the |>* macros.

The snippet above expands to:

```clojure
(into (empty employee) ;; Maintains the return type
      (comp (r/join dept {:dept-name :name})
            (r/aggregate-over :dept-name {:dept-colleagues [r/vec-agg :name]})
            (r/update :dept-colleagues count)
            (r/sort-by :emp-id))
      employee)

```


### Aggregations

As seen in the example we can aggregate relations. Relation comes with a few built in aggregation functions:

* set-agg - Aggregates values into a set
* vec-agg - Aggregates values into a vector
* stats-agg - Aggregates simple stats like min, max, count, avg and sum

All aggregations are transducing functions which means that we can easily use any of the statistics from [MastodonC/kixi.stats](https://github.com/MastodonC/kixi.stats)

``` clojure
(require '[kixi.stats.core :as stats])

(|> employee
    (r/aggregate {:emp-id-stddev [stats/standard-deviation :emp-id]}))

=> ({:emp-id-stddev 915.1378038306581})
```

### Extending

The `|>`, `|>seq`, `|>set`, `|>vec`, `|>normalized` macros compose a transducing process so each step can be exteded using any transducer.

``` clojure

(|> employee
    ;; Any normal transducer works here
    (filter (comp odd? :emp-id))
    ;; And here..
    (map (fn [row] (update row :emp-id #(str "subsidiary-" %))))
    (r/project [:emp-id]))

=> ({:emp-id "subsidiary-3415"}
    {:emp-id "subsidiary-1257"}
    {:emp-id "subsidiary-2241"}
    {:emp-id "subsidiary-3401"})
```
