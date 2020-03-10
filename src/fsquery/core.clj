(ns fsquery.core
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.string :as string]
   [fsquery.fsnode :as fsnode]
   ))


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
  {:init-path (-> path io/file .getAbsolutePath)
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
               (let [dir-children (->> (fsnode/children fsnode)
                                       (filter fsnode/is-dir?)
                                       (filter #(not ((dir-tests fsquery) %))))

                     file-children (filter (file-tests fsquery)
                                    (filter #(not (fsnode/is-dir? %))
                                            (fsnode/children fsnode))) ]
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
    (walk 0 (fsnode/make init-path init-path 0))))

(defn walk-each [fsq] (start-walk fsq))


;; Standard features
;; __________________________________________________________________________

(defn files-only [fsquery]
  (add-return-criteria fsquery #(not (fsnode/is-dir? %))))

(defn dirs-only [fsquery]
  (add-return-criteria fsquery #(fsnode/is-dir? %)))

(defn no-follow [fsquery pattern]
  (add-dir-pred fsquery #(if (re-find (re-pattern pattern) (:abs %)) true false )))

(defn match [fsquery pattern]
  (add-file-pred fsquery #(if (re-find (re-pattern pattern) (:abs %)) true false)))

(defn ext [fsquery s]
  (match fsquery (re-pattern (str s "$"))))


(defn file-contains [fsquery pattern]
  (add-file-pred fsquery #(contains? % (re-pattern pattern)))
  )


;;  Extra functionality for nodes
;; Spawn a new fsquery

(defn spawn-query [{:keys [ root depth java-file]}] (make-fsquery (-> java-file :abs)))
