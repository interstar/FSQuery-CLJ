(defproject fsquery "0.1.0-SNAPSHOT"
  :description "FSQuery : Make the File System like JQuery. And then put it under core.logic control"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.logic "0.8.11"]
                 [expound "0.7.1"]

                 ]

  :main fsquery.core
  :aot [fsquery.core]
  )
