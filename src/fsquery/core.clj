(ns fsquery.core
  (:require
   [clojure.java.io :as io]

   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.string :as str]

   ))



;; ____ FSNode ____________________________________________________________________
(s/def ::java-file #(instance? java.io.File % ))
(s/def ::abs string?)
(s/def ::root string?)
(s/def ::depth number?)

(s/def ::FSNode (s/keys :req-un [::abs ::depth ::root ::java-file]) )

(defn make-fsnode [abs root depth]
  {:abs abs :root root :depth depth :java-file (io/file abs)})

(s/fdef make-fsnode :ret ::FSNode)
(stest/instrument `make-fsnode)

(defn is-dir? [{:keys [abs root depth java-file]}]
  (.isDirectory  java-file))


(defn children [{:keys [abs root depth java-file] :as fsnode}]
  (if (is-dir? fsnode)
    (map #(make-fsnode (.getAbsolutePath %) root (+ depth 1) ) (.listFiles java-file))
    []))

(defn has-child? [fsnode p]
  (if (some true? (map p (children fsnode))) true false))


(defn slurp-it [fsnode] (slurp (:abs fsnode)))
(defn spit-it [fsnode s] (spit (:abs fsnode) s))

(defn contains? [fsnode pattern]
  (let [s (slurp-it fsnode)]
    (if (nil? (re-find pattern s)) false true)))

(defn file-object [fsnode] (:java-file fsnode))

(declare make-fsquery)
(defn spawn-query [{:keys [abs root depth java-file]}] (make-fsquery abs))

(defn has-child-named? [node name]
  (let [name-last? (fn [child]
                       (= (-> child
                              :abs
                              (#(str/split % #"/"))
                              last)
                          name
                          ))

          ]
      (and
       (is-dir? node)
       (has-child? node name-last?)
       )))

;; ____ FSQuery __________________________________________________________________

(s/def ::pred (s/fspec :args (s/tuple any?) :ret boolean?))
(s/def ::pred-coll (s/coll-of ::pred))

(s/def ::init-path string?)
(s/def ::dir-includes (s/coll-of any?))
(s/def ::file-includes (s/coll-of any?))
(s/def ::return-criteria (s/coll-of any?))
(s/def ::FSQuery (s/keys :req-un [::init-path ::dir-preds
                                  ::file-preds ::return-criteria]))

(defn make-fsquery [path]
  {:init-path path
   :dir-preds []
   :file-preds []
   :return-criteria []}
  )


(defn add-pred [fsquery type p]
  {:pre [(s/valid? ::FSQuery fsquery)]
   :post [(s/valid? ::FSQuery %)]}
  (conj fsquery {type (cons p (get fsquery type))}))



;; dir predicates are about EXCLUDING
;; if ANY predicate is triggered, we exclude
(defn add-dir-pred [fsquery p] (add-pred fsquery :dir-preds p))

;; file predicates are about INCLUDING
;; ALL predicates need to be true to include
(defn add-file-pred [fsquery p] (add-pred fsquery :file-preds p))

;; return-criteria EXCLUDE
;; if ANY predicate is true, we exclude from showing (but not necessarily from showing children)
(defn add-return-criteria [fsquery p]
  (add-pred fsquery :return-criteria p))


(defn dir-tests [fsquery]
  (let [f (fn [n]
            {:pre (s/valid? ::FSNode n)}
            (let [bs (map #(% n) (:dir-preds fsquery))]
              (if (some true? bs) true false)))]
    f))

(defn file-tests [fsquery]
  (let [f (fn [n]
            {:pre (s/valid? ::FSNode n)}
            (let [bs (map #(% n) (:file-preds fsquery))]
              (every? true? bs)))]
    f))

(defn return-tests [fsquery]
  (let [f (fn [n]
            (if (empty? (:return-criteria fsquery)) true
                (let [bs (map #(% n) (:return-criteria fsquery))]
                  (every? true? bs))))]
    f))

(defn start-walk [{:keys [init-path dir-preds file-preds return-criteria]
                   :as fsquery} ]
  (letfn [ (walk [depth fsnode]
               (let [dir-children (->> (children fsnode)
                                       (filter is-dir?)
                                       (filter #(not ((dir-tests fsquery) %))))

                     file-children (filter (file-tests fsquery)
                                    (filter #(not (is-dir? %))
                                            (children fsnode))) ]
                 (filter #((return-tests fsquery) %)
                         (lazy-cat [fsnode]
                                   (apply concat
                                    (map
                                     #(walk (inc depth) %)
                                     file-children))
                                   (apply concat
                                    (map
                                     #(walk (inc depth) %)
                                     dir-children)) ))
                 ))]
    (walk 0 (make-fsnode init-path init-path 0))))




;; Standard features
;; __________________________________________________________________________

(defn files-only [fsquery]
  (add-return-criteria fsquery #(not (is-dir? %))))

(defn dirs-only [fsquery]
  (add-return-criteria fsquery #(is-dir? %)))

(defn no-follow [fsquery pattern]
  (add-dir-pred fsquery #(if (re-find (re-pattern pattern) (:abs %)) true false )))

(defn match [fsquery pattern]
  (add-file-pred fsquery #(if (re-find (re-pattern pattern) (:abs %)) true false)))

(defn ext [fsquery s]
  (match fsquery (re-pattern (str s "$"))))


(defn file-contains [fsquery pattern]
  (add-file-pred fsquery #(contains? % (re-pattern pattern)))
  )
