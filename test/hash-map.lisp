;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   test/hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl/test)

(def-suite :hash-map-tests
  :description "testing hash map operations"
  :in master-suite)

(in-suite :hash-map-tests)

(named-readtables:in-readtable persidastricl:syntax)

(test simple-persistent-hash-map-creation-test
  (let ((m {:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep m 'persistent-hash-map))
    (is (= 4 (count m)))
    (is (== :v1 (lookup m :k1)))
    (is (== :v2 (lookup m :k2)))
    (is (== :v3 (lookup m :k3)))
    (is (== :v4 (lookup m :k4)))))

(test simple-transient-hash-map-creation-test
  (let ((m @{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep m 'transient-hash-map))
    (is (= 4 (count m)))
    (is (== :v1 (lookup m :k1)))
    (is (== :v2 (lookup m :k2)))
    (is (== :v3 (lookup m :k3)))
    (is (== :v4 (lookup m :k4)))))

(test hash-map-assoc-test
  (let ((m1 {})
        (m2 @{}))
    (is (== {:a 1 :b 2} (assoc m1 :a 1 :b 2)))
    (is (== @{:a 1 :b 2} (assoc m2 :a 1 :b 2)))))

(test hash-map-dissoc-test
  (let ((m1 {:a 1 :b 2})
        (m2 @{:a 1 :b 2}))
    (is (== {:b 2} (dissoc m1 :a)))
    (is (== @{:b 2} (dissoc m2 :a)))))

(test hash-map-as-plist
  (is (== '(:a 1 :b 2) (into '() (->plist {:a 1 :b 2})))))

(test hash-map-as-alist
  (let ((a-list (->alist {:a 1 :b 2})))
    (is (every? #'consp a-list))
    (is (== (set '((:a . 1) (:b . 2))) (set a-list)))))

(test hash-map-as-list
  (is (== (list (p::map-entry :a 1) (p::map-entry :b 2))
          (->list {:a 1 :b 2}))))

(test using-into-with-hash-maps-and-tables
  (is  (true?
        (reduce
         (fn (m1 m2)
           (when (== m1 m2) m1))
         [(into %{} '((:a . 1) (:b . 2) (:c . 3)))
          (into %{} '((:a  1) (:b  2) (:c  3)))
          (into %{} {:a 1 :b 2 :c 3})
          (into %{} @{:a 1 :b 2 :c 3})
          (into %{} %{:a 1 :b 2 :c 3})
          (into %{} [[:a 1] [:b 2] [:c 3]])
          (into %{} [(p::map-entry :a 1) (p::map-entry :b 2) (p::map-entry :c 3)])
          (into %{} #{'(:a 1) '(:b 2) '(:c 3)})
          (into %{} #{'(:a . 1) '(:b . 2) '(:c . 3)})])))

  (is  (true?
        (reduce
         (fn (m1 m2)
           (when (== m1 m2) m1))
         [(into @{} '((:a . 1) (:b . 2) (:c . 3)))
          (into @{} '((:a  1) (:b  2) (:c  3)))
          (into @{} {:a 1 :b 2 :c 3})
          (into @{} @{:a 1 :b 2 :c 3})
          (into @{} %{:a 1 :b 2 :c 3})
          (into @{} [[:a 1] [:b 2] [:c 3]])
          (into @{} [(p::map-entry :a 1) (p::map-entry :b 2) (p::map-entry :c 3)])
          (into @{} #{'(:a 1) '(:b 2) '(:c 3)})
          (into @{} #{'(:a . 1) '(:b . 2) '(:c . 3)})])))

  (is  (true?
        (reduce
         (fn (m1 m2)
           (when (== m1 m2) m1))
         [(into {} '((:a . 1) (:b . 2) (:c . 3)))
          (into {} '((:a  1) (:b  2) (:c  3)))
          (into {} {:a 1 :b 2 :c 3})
          (into {} @{:a 1 :b 2 :c 3})
          (into {} %{:a 1 :b 2 :c 3})
          (into {} [[:a 1] [:b 2] [:c 3]])
          (into {} [(p::map-entry :a 1) (p::map-entry :b 2) (p::map-entry :c 3)])
          (into {} #{'(:a 1) '(:b 2) '(:c 3)})
          (into {} #{'(:a . 1) '(:b . 2) '(:c . 3)})]))))

;;(5am:run! :hash-map-tests)
