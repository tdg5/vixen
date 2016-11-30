(ns vixen.core-test
  (:refer-clojure :exclude [remove find any class])
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [vixen.core :as v]))

(def h
  "<html>
     <head>
       <title>Vixens are female foxes</title>
     </head>
     <body>
       <h1>All about vixens</h1>
       <p class='intro'>
         <b>Vixens</b> are female foxes and also a glam metal band from Minnesota.
       </p>
       <p class='more cheese'>
         This should really contain more information about vixens, but I don't know any.
       </p>
       <h2 class='midway'>but wait there's more</h2>
       <div class='more'></div>
       <div data-lasers='xenon' class='cheese'>Zounds</div>
       <div data-lasers='argon'>
         <p class='more'>goats</p>
       </div>
      </body>
     </html>")

(defn parsed [] (v/parse-string h))

(deftest parse
  (testing "parsing a string"
    (let [parsed (v/parse-string h)
          doc (:doc parsed)
          selector (:selector parsed)]

      (is (empty? selector))
      (is (map? doc))
      (is (set (keys doc)) #{:tag :content :attrs}))))

(deftest filtering-sanity
  (testing "empty filter"
    (let [filtered (v/select (parsed))]
      (is (= (v/nodes (parsed)) (v/nodes filtered))))))

(deftest children
  (testing "children from the top"
    (let [children (->
                    (parsed)
                    (v/select v/child)
                    v/nodes)]
      (is (= (count children) 3)) ;includes a sort of weird text node
      (is (= (-> children first :tag) :head))
      (is (= (-> children (nth 2) :tag) :body)))))

(deftest elems
  (testing "only get body"
    (let [bod (->
               (parsed)
               (v/select (v/sub (v/elem= :body)))
               v/nodes)]
      (is (= (count bod) 1))
      (is (= (-> bod first :tag) :body))))

  (testing "nested"
    (let [p (->
             (parsed)
             (v/select (v/sub (v/elem= :body)) (v/sub (v/elem= :p)))
             v/nodes)]
      (is (= (count p) 2))
      (is (= (-> p first :tag) :p)))))

(deftest class
  (testing "basic"
    (let [more
          (->
           (parsed)
           (v/select (v/sub (v/elem= :body)) (v/sub (v/class= :more)))
           v/nodes)]
      (is (= (count more) 2))
      (is (= (-> more first :tag) :p))
      (is (= (-> more second :tag) :div))))

  (testing "composed and"
    (let [more (->
                (parsed)
                (v/select (v/sub (v/elem= :body))
                          (v/sub (v/and-fn (v/class= :more) (v/elem= :p))))
                v/nodes)]
      (is (= (count more) 1))
      (is (= (-> more first :tag) :p))))

  (testing "composed or"
    (let [more (v/nodes (v/select (parsed)
                                  (v/sub (v/elem= :body))
                                  (v/sub (v/or-fn (v/class= :more) (v/elem= :p)))))]
      (is (= (count more) 3))))

  (testing "multiple"
    (let [more (v/nodes (v/select (parsed)
                                  (v/sub (v/elem= :body))
                                  (v/sub (v/class= :cheese))))]
      (is (= (count more) 2))
      (is (= (-> more first :tag) :p))
      (is (= (-> more second :tag) :div)))))

(deftest text)

(deftest find

  (testing "basic"
    (let [more (v/nodes (v/find (parsed) (v/class= :more)))]
      (is (= (count more) 3))
      (is (= (:content (nth more 2)) ["goats"]))))

  (testing "elements"
    (let [ps (v/nodes (v/find (parsed) (v/elem= :p)))]
      (is (= (count ps) 3))
      (is (every? #{:p} (map :tag ps)))
      (is (= (:content (nth ps 2)) ["goats"]))))

  (testing "paths"
    (let [paths (v/paths (v/find (parsed) (v/class= :more)))]

      (is (= (first paths) [2 5]))
      (is (= (second paths) [2 9]))
      (is (= (nth paths 2) [2 13 1]))))

  (testing "missing"
    (let [nodes (v/nodes (v/find (parsed) (v/class= :glorb)))]
      (is (empty? nodes))))

  (testing "text"
    (let [glam (v/nodes (v/find (parsed) (v/text= #"glam")))]
      (is (= (count glam) 1))
      (is (= (string/trim (first glam)) "are female foxes and also a glam metal band from Minnesota.")))))

(deftest parent
  (testing "deep"
    (let [goats (-> (parsed)
                    (v/find (v/text= #"goats"))
                    (v/select v/parent v/parent)
                    v/nodes)]

      (is (= (count goats) 1))
      (is (= (-> goats first :attrs :data-lasers) "argon"))))

  (testing "multiple"

    (let [more (-> (parsed)
                   (v/find (v/class= :more))
                   (v/select v/parent)
                   v/nodes)]

      (is (= (count more) 2))
      (is (= (:tag (first more)) :body))
      (is (= (:tag (second more)) :div))))


  (testing "top"

    (let [more (-> (parsed)
                   (v/select v/parent)
                   v/nodes)]

      (is (empty? more)))))

(deftest siblings
  (testing "none"
    (let [title (-> (parsed)
                    (v/select (v/sub (v/elem= :title))
                              v/siblings)
                    v/nodes)]
      (is (empty? title))))

  (testing "lots"
    (let [midway (-> (parsed)
                     (v/select (v/sub (v/elem= :body))
                               (v/sub (v/class= :midway))
                               v/siblings)
                     v/nodes)
          trimmed (filter :tag midway)]
      (is (= (count trimmed) 6))))

  (testing "before"
    (let [midway (-> (parsed)
                     (v/select (v/sub (v/elem= :body))
                               (v/sub (v/class= :midway))
                               v/siblings-before)
                     v/nodes)
          trimmed (filter :tag midway)]
      (is (= (count trimmed) 3))
      (is (= (:tag (first trimmed)) :h1))))

  (testing "after"
    (let [midway (-> (parsed)
                     (v/select (v/sub (v/elem= :body))
                               (v/sub (v/class= :midway))
                               v/siblings-after)
                     v/nodes)
          trimmed (filter :tag midway)]
      (is (= (count trimmed) 3))
      (is (= (:tag (first trimmed)) :div)))))

(deftest concrete)

(deftest to-html)

(deftest to-text
  (testing "whole thing"
    (is (= (v/to-text (parsed))
           "Vixens are female foxes All about vixens Vixens are female foxes and also a glam metal band from Minnesota. This should really contain more information about vixens, but I don't know any. but wait there's more  Zounds goats" ))))

(deftest reset)

(deftest remove
  (testing "by class"
    (let [unmore (->
                  (parsed)
                  (v/find (v/class= "more"))
                  v/remove
                  (v/find (v/class= "more"))
                  v/nodes)]
      (is (empty? unmore)))))

(deftest set-attr
  (testing "each paragraph"
    (let [ps (v/find (parsed) (v/elem= :p))
          updated (v/set-attr ps :lasers "ghouls")
          nodes (v/nodes updated)]
      (is (= 3 (count nodes)))
      (is (every? #(= (-> % :attrs :lasers) "ghouls") nodes)))))

(deftest append)

(deftest prepend)

(deftest split
  (testing "lots"
    (let [found (v/find (parsed) (v/elem= :p))
          s (v/split found)]

      (is (= 3 (count s)))

      (let [n (v/nodes (first s))]
        (is (= 1 (count n)))
        (is (= :p (:tag (first n))))))))

(run-tests)
