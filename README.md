1
# FSQuery (File System Query)

Working with the file-system is too verbose. Let's make it more like JQuery

This is a port / adaptation of the Python library : [https://github.com/interstar/fsquery](https://github.com/interstar/fsquery)


[![Clojars Project](https://img.shields.io/clojars/v/fsquery.svg)](https://clojars.org/fsquery)

## Usage

We create an FSQuery record using `make-fsquery` and the path as argument.

We can then attach various further predicates and modifiers to it.

Finally we use `start-walk` to create a lazy sequence of the FSNodes mateched by the query. 



```
(let [fsq (-> (make-fsquery "~/my-projects")
              (files-only)
              (ext "clj")
              (no-follow #"\.git\/")
              (no-follow #"\/vendor\/")]

      (doseq [x (start-walk fsq)]
        (println (:abs x)))))

```

## License

Copyright © 2018 Phil Jones

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
