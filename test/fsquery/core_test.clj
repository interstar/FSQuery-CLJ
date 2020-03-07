(ns fsquery.core-test
  (:require [clojure.test :refer :all]
            [fsquery.fsnode :as fsnode]
            [fsquery.core :refer [make-fsquery
                                   add-file-pred
                                   add-return-criteria add-dir-pred
                                   files-only no-follow match
                                   ext file-contains
                                   dirs-only
                                  start-walk dir-tests file-tests]
             :as fsquery]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.java.shell :refer [sh]]
            [pathetic.core :as pathetic]
            ))





(deftest fsnode-test
  (testing "FSNode"
    (is (s/valid? ::fsnode/FSNode (fsnode/make "/" "/" 0)))
    (is (true? (fsnode/has-child? (fsnode/make "/" "/" 0) #(= "/tmp" (fsnode/abs %)))))

    )
  )


(deftest CleanPath
  (testing "CleanPath"
    (is (= (fsnode/clean-path "/clean/path") "/clean/path"))
    (is (= (fsnode/clean-path "./dirty/path") "dirty/path"))
    (is (= (fsnode/clean-path "hello/./path") "hello/path")))
  )


(testing "Dir Create and Crawling"
    (let [ds
          [
           "play/"
           "play/A"
           "play/A/a"
           ["play/A/a/a.txt" "this is a.txt"]
           ["play/A/a/b.txt" "this is b.txt"]
           "play/B"
           "play/B/cc"
           ["play/B/cc/c.out" "this is c.out"]
           "play/B/dd"
           "play/old"
           "play/old/a"
           "play/old/a/x"
           ["play/old/a/x/data.txt" "this is data.txt"]
           ["play/old/a/x/test.html" "this is test.html"]
           "play/old/a/y"
           ["play/old/a/y/data.txt" "this is another data.txt"]
           "play/old/b"
           "play/old/b/ANOTHER FAMILY"
           "play/old/b/NEW FAMILY"
           ["play/old/b/NEW FAMILY/data.text" "this is yet more data.text"]
           ]]
      (fsnode/list-to-dir!! "./play" ds)

      (deftest test-crawl
        (testing "test-crawl"
          (let [
                fsq (make-fsquery "play")
                res (map #(fsnode/relative %) (fsquery/walk-each fsq))]
            (is (= res ds)))))

      (deftest dirs
        (testing "dirs only"
          (let [fsq (-> (make-fsquery "play")
                        (dirs-only)
                        (no-follow #"A/"))]

            (doseq [x (start-walk fsq)]
              (println "?? " (fsnode/abs x))))))



      (deftest fsquery
        (let [
              f #(true? %)
              f2 #(false? %)
              f3 #(true? %)
              fq (-> "/" make-fsquery
                     (add-dir-pred f)
                     (add-return-criteria f3)
                     (add-file-pred f2)
                     )]



          (testing "FSQuery"
            (is (s/valid? ::fsquery/FSQuery fq))
            (is (= (-> fq :dir-preds first) f))
            (is (= (-> fq :file-preds first) f2))
            (is (= (-> fq :return-criteria first) f3)))

          (testing "Go"
            (let [
                  fsq (make-fsquery "./play")
                  fsq2 (-> fsq (add-dir-pred #(.contains (fsnode/abs %) "B")))
                  fsq3 (-> fsq
                           (add-dir-pred #(.contains (fsnode/abs %) "cc"))
                           (add-dir-pred #(.contains (fsnode/abs %) "dd"))
                           )
                  fsq4 (-> fsq (add-file-pred #(.contains (fsnode/abs %) ".out")))
                  fsq5 (-> fsq4 (add-return-criteria #(not (fsnode/is-dir? %))))
                  fsq6 (-> fsq files-only)
                  fsq7 (-> fsq6 (no-follow "B"))
                  fsq8 (-> fsq6 (match #"\.out"))
                  fsq9 (-> fsq6 (ext "out"))
                  fsq10 (-> fsq6 (file-contains "cruel"))
                  ]
              (doseq [x (start-walk fsq)]
                (println "--> " (fsnode/abs x))
                )

              (is (= false ((dir-tests fsq2) {fsnode/abs "abc"})) )
              (is (= true ((dir-tests fsq2) {fsnode/abs "aBc"})))
              (doseq [x (start-walk fsq2)]
                (println "++> " (fsnode/abs x)))
              (doseq [x (start-walk fsq3)]
                (println "__> " (fsnode/abs x)))

              (is (= false ((file-tests fsq4) {fsnode/abs "abc.txt"})) )
              (is (= true ((file-tests fsq2) {fsnode/abs "aBc.out"})))

              (doseq [x (start-walk fsq4)]
                (println "^^> " (fsnode/abs x)))

              (doseq [x (start-walk fsq5)]
                (println "..> " (fsnode/abs x)))

              (doseq [x (start-walk fsq6)]
                (println "((> " (fsnode/abs x)))

              (doseq [x (start-walk fsq7)]
                (println "))> " (fsnode/abs x)))

              (doseq [x (start-walk fsq8)]
                (println "==> " (fsnode/abs x)))

              (doseq [x (start-walk fsq9)]
                (println "=+> " (fsnode/abs x)))

              (doseq [x (start-walk fsq10)]
                (println "0+> " (fsnode/abs x)))

              ))))

      (comment
        (deftest showfiles
          (testing "quick grep"
            (let [fsq (->
                       (make-fsquery "./")
                       (ext "clj")
                       files-only)]
              (doseq [x (start-walk fsq)]

                (println "------------------------------------------")
                (println (fsnode/abs x))
                (println "------------------------------------------")
                (let [s (fsnode/slurp-it x)]
                  (doseq [y (string/split-lines s)]
                    (println y))))
              ))
          ))

      )
    )
