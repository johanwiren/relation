# Relation

## Rationale

Relational algebra in Clojure should be easy, fast and composable.
And it should work for both clojure and clojurescript.

Well "fast" as in not slower than clojure.set ..

## Usage

``` clojure
(require '[johanwiren.se.relation :as r])

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

(-> (r/relation employee)
    (r/join (r/relation dept) {:dept-name :dept-name})
    r/set)
    
=> 
#{{:name "George", :emp-id 3401, :dept-name "Finance", :manager "George"}
  {:name "Harriet", :emp-id 2202, :dept-name "Sales", :manager "Harriet"}
  {:name "Harry", :emp-id 3415, :dept-name "Finance", :manager "George"}
  {:name "Sally", :emp-id 2241, :dept-name "Sales", :manager "Harriet"}}

```

Note that we're using `->` to thread the relation in first position.
This gives us more options for expressing further operations.

Under the hood relation uses a reducing process and composes most
operations as a transducer. That is why we need the `r/set` in the
end so that relation knows what to return.

There is a shorthand macro `|>` that works like `->` and inserts the
`r/set` in the end for you

### Operations

Relation provides a rich set of operations

* select
* assoc
* dissoc
* rename
* extend
* update
* project
* join
* left-join
* right-join
* aggregate-by
* aggregate-over
* aggregate
* sort-by
* union
* difference
* intersection
* extend-kv
* expand-kv
* expand-seq

And they compose easily like this:

``` clojure
(|> (r/relation employee)
    (r/join (r/relation dept) {:dept-name :dept-name})
    (r/aggregate-over :dept-name {:dept-colleagues [r/vec-agg :name]})
    (r/update :dept-colleagues count)
    (r/sort-by :emp-id))
=>
#{{:name "Harriet",
   :emp-id 2202,
   :dept-name "Sales",
   :manager "Harriet",
   :dept-colleagues 2}
  {:name "Sally",
   :emp-id 2241,
   :dept-name "Sales",
   :manager "Harriet",
   :dept-colleagues 2}
  {:name "George",
   :emp-id 3401,
   :dept-name "Finance",
   :manager "George",
   :dept-colleagues 2}
  {:name "Harry",
   :emp-id 3415,
   :dept-name "Finance",
   :manager "George",
   :dept-colleagues 2}}
```

### Aggregations

As seen in the example we can aggregate relations. Relation
comes with a few build in aggregation functions:

* set-agg - Aggregates values into a set
* vec-agg - Aggregates values into a vector
* stats-agg - Aggregates simple stats like min, max, count, avg and sum

All aggregations are transducing functions which means that we can easily
use any of the statistics from [MastodonC/kixi.stats](https://github.com/MastodonC/kixi.stats)

### Extending

Since everything is built on tranducers you can easily add your own steps.

``` clojure

(def xform
  (map (fn [row] (update row :emp-id #(str "subsidiary-" %)))))

(|> (r/relation employee)
    (r/comp xform)
    (r/project [:emp-id]))
    
=>
#{{:emp-id "subsidiary-2241"}
  {:emp-id "subsidiary-3415"}
  {:emp-id "subsidiary-2202"}
  {:emp-id "subsidiary-1257"}
  {:emp-id "subsidiary-3401"}}
```

Or use `r/comp` which most of relation's operations are built upon to make your own
named operation.

``` clojure
(defn make-subsidiary [rel]
  (r/comp rel (map (fn [row] (update row :emp-id #(str "subsidiary-" %))))))

(|> (r/relation employee)
    (r/sort-by :emp-id)
    make-subsidiary
    (r/project [:emp-id]))

#{{:emp-id "subsidiary-1257"}
  {:emp-id "subsidiary-2202"}
  {:emp-id "subsidiary-2241"}
  {:emp-id "subsidiary-3401"}
  {:emp-id "subsidiary-3415"}}
```
