(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(group-by :name [{:name "mdp" :data 1} {:name "jbm" :data 1000}])

(lmap :id [{:id {:a 1} :d 1} {:id {:a 1} :d 2} {:id 1 :d 3} {:id 2 :d 2} {:id 2 :d 1} {:id 3 :d 2} {:id 4 :d 1} {:id 5 :d 2} ])

(mapv :id [{:a 1 :id {:a 1} :d 1} {:id {:a 1} :d 2} {:id 1 :d 3} {:id 2 :d 2} {:id 2 :d 1} {:id 3 :d 2} {:id 4 :d 1} {:id 5 :d 2} ] )

(lmap
 (juxt :a :d)
 [{:a 1 :id {:a 1} :d 1} {:id {:a 1} :d 2} {:id 1 :d 3} {:id 2 :d 2} {:id 2 :d 1} {:id 3 :d 2} {:id 4 :d 1} {:id 5 :d 2} ] )

(:id {} :not-found)

(defvar m1 {:a 5 :b 2})

(with-funcallable-map (m2 m1)
  (m2 :a))

(m2 :a)

(defvar s1 #{:eagle :falcon :hawk :vulture})

(with-funcallable-set (s2 s1)
  (s2 :eagle))

(s2 :eagle)
