(ns fsquery.fsnode
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.string :as string]
   [pathetic.core :as pathetic]
   [clojure.java.shell :refer [sh]]
   ))



;; ____ FSNode ____________________________________________________________________
(s/def ::java-file #(instance? java.io.File % ))
(s/def ::depth number?)
(s/def ::root string?)

(s/def ::FSNode (s/keys :req-un [::depth ::root ::java-file]) )

(defn clean-path [path]
  (pathetic/normalize path)
  )

(defn make [abs root depth]
  {:java-file (io/file abs)
   :root root
   :depth depth})

(s/fdef make :ret ::FSNode)
(stest/instrument `make)


(defn abs [{:keys [java-file]}]
  (-> java-file .getAbsolutePath clean-path))

(defn relative [{:keys [java-file root] :as node} ]
  (string/replace (-> node abs) root ""))

(defn file-name [{:keys [java-file]}]
  (.getName java-file))

(defn is-dir? [{:keys [abs root depth java-file]}]
  (.isDirectory  java-file))


(defn exists? [{:keys [abs root depth java-file]} ]
  (.exists java-file))

(defn children [{:keys [abs root depth java-file] :as fsnode}]
  (if (is-dir? fsnode)
    (map #(make (.getAbsolutePath %) root (+ depth 1) ) (.listFiles java-file))
    []))

(defn has-child? [fsnode p]
  (if (some true? (map p (children fsnode))) true false))


(defn slurp-it [node] (slurp (abs node)))
(defn spit-it [node s] (spit (abs node) s))

(defn contains? [fsnode pattern]
  (let [s (slurp-it fsnode)]
    (if (nil? (re-find pattern s)) false true)))

(defn file-object [fsnode] (:java-file fsnode))




(defn has-child-named? [node name]
  (let [name-last? (fn [child]
                       (= (-> child
                              :abs
                              (#(string/split % #"/"))
                              last)
                          name
                          ))]
      (and
       (is-dir? node)
       (has-child? node name-last?)
       )))


(defn list-to-dir!! [root xs]
  (do
    (sh "rm" "-r" root)
    (doseq [x xs]
      (println "WWWW " x)
      (if (vector? x)
        (let [[fname cont] x]
          (println "IS VECTOR '" fname "' ::: '" cont "'")
          (spit fname cont))
        (sh "mkdir" "-p" x))
      )
    ))
