(ns fsquery.core-test
  (:require [clojure.test :refer :all]
            [fsquery.core :refer [make-fsquery make-fsnode
                                   has-child? add-file-pred
                                   add-return-criteria add-dir-pred
                                   is-dir? files-only no-follow match
                                   ext file-contains slurp-it
                                   dirs-only
                                  start-walk dir-tests file-tests]
             :as core ]
            [clojure.spec.alpha :as s]
              [clojure.string :as str]
            ))



(deftest fsnode
  (testing "FSNode"
    (is (s/valid? ::core/FSNode (make-fsnode "/" "/" 0)))
    (is (true? (has-child? (make-fsnode "/" "/" 0) #(= "/tmp" (:abs %)))))

    ))

(deftest fsquery
  (let [f #(true? %)
        f2 #(false? %)
        f3 #(true? %)
        fq (-> "/" make-fsquery
               (add-dir-pred f)
               (add-return-criteria f3)
               (add-file-pred f2)
               )]

    (testing "FSQuery"
      (is (s/valid? ::core/FSQuery fq))
      (is (= (-> fq :dir-preds first) f))
      (is (= (-> fq :file-preds first) f2))
      (is (= (-> fq :return-criteria first) f3)))

    (testing "Go"
      (let [fsq (make-fsquery "./play")
            fsq2 (-> fsq (add-dir-pred #(.contains (:abs %) "B")))
            fsq3 (-> fsq
                     (add-dir-pred #(.contains (:abs %) "cc"))
                     (add-dir-pred #(.contains (:abs %) "dd"))
                     )
            fsq4 (-> fsq (add-file-pred #(.contains (:abs %) ".out")))
            fsq5 (-> fsq4 (add-return-criteria #(not (is-dir? %))))
            fsq6 (-> fsq files-only)
            fsq7 (-> fsq6 (no-follow "B"))
            fsq8 (-> fsq6 (match #"\.out"))
            fsq9 (-> fsq6 (ext "out"))
            fsq10 (-> fsq6 (file-contains "cruel"))
            ]
        (doseq [x (start-walk fsq)]
          (println "--> " (:abs x))
          )

        (is (= false ((dir-tests fsq2) {:abs "abc"})) )
        (is (= true ((dir-tests fsq2) {:abs "aBc"})))
        (doseq [x (start-walk fsq2)]
          (println "++> " (:abs x)))
        (doseq [x (start-walk fsq3)]
          (println "__> " (:abs x)))

        (is (= false ((file-tests fsq4) {:abs "abc.txt"})) )
        (is (= true ((file-tests fsq2) {:abs "aBc.out"})))

        (doseq [x (start-walk fsq4)]
          (println "^^> " (:abs x)))

        (doseq [x (start-walk fsq5)]
          (println "..> " (:abs x)))

        (doseq [x (start-walk fsq6)]
          (println "((> " (:abs x)))

        (doseq [x (start-walk fsq7)]
          (println "))> " (:abs x)))

        (doseq [x (start-walk fsq8)]
          (println "==> " (:abs x)))

        (doseq [x (start-walk fsq9)]
          (println "=+> " (:abs x)))

        (doseq [x (start-walk fsq10)]
          (println "0+> " (:abs x)))

        ))))

(deftest showfiles
  (testing "quick grep"
    (let [fsq (->
               (make-fsquery "./")
               (ext "clj")
               files-only)]
      (doseq [x (start-walk fsq)]

        (println "------------------------------------------")
        (println (:abs x))
        (println "------------------------------------------")
        (let [s (slurp-it x)]
          (doseq [y (str/split-lines s)]
            (println y))))
      ))
    )

(deftest dirs
  (testing "dirs only"
    (let [fsq (-> (make-fsquery "./")
                  (dirs-only)
                  (no-follow #"\.git\/")
                  (no-follow #"target\/"))]

      (doseq [x (start-walk fsq)]
        (println (:abs x))))))
