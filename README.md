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

(def employee #{{:name "Harry" :emp-id 3415 :dept-name "Finance" :age 57}
                {:name "Sally" :emp-id 2241 :dept-name "Sales" :age 26}
                {:name "George" :emp-id 3401 :dept-name "Finance" :age 42}
                {:name "Harriet" :emp-id 2202 :dept-name "Sales" :age 54}
                {:name "Mary" :emp-id 1257 :dept-name "Human Resources" :age 35}})
                
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

### Aggregations

Relation has a few built in aggregation functions:

* set-agg - Aggregates values into a set
* vec-agg - Aggregates values into a vector
* stats-agg - Aggregates simple stats like min, max, count, avg and sum

The aggregations all expect transducing functions which means that we you can easily write your own or use any of the statistics from [MastodonC/kixi.stats](https://github.com/MastodonC/kixi.stats)

``` clojure
(require '[kixi.stats.core :as stats])

(|>first employee
    (r/aggregate {:stddev-age [stats/standard-deviation :age]
                  ;; Plain old clojure.core/+
                  :total-age [+ :age]}))

=> {:stddev-age 12.949903474543738, :total-age 214}
```

### Extending

The `|>`, `|>seq`, `|>set`, `|>vec`, `|>normalized` functions compose a transducing process so each step can be exteded using any transducer.

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
