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
;;; test/walk.lisp
;;;
;;;  testing walk functions
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :walk-tests
  :description "testing walk package functions"
  :in master-suite)

(in-suite :walk-tests)

(test prewalk-replace-test
  (is (== (walk:prewalk-replace {:a :b} [:a {:a :a} '(3 :c :a)])
          [:b {:b :b} '(3 :c :b)])))

(test postwalk-replace-test
  (is (== (walk:postwalk-replace {:a :b} [:a {:a :a} '(3 :c :a)])
          [:b {:b :b} '(3 :c :b)])))

(test stringify-keys-test
  (is (== (walk:stringify-keys {:a 1 nil {:b 2 :c 3} :d 4})
          {"a" 1 nil {"b" 2 "c" 3} "d" 4})))

(test keywordize-keys-test
  (is (== (walk:keywordize-keys {"a" 1 nil {"b" 2 "c" 3} "d" 4})
          {:a 1 nil {:b 2 :c 3} :d 4})))


(test prewalk-order-test
  (let ((a (atom [])))
    (walk:prewalk (lambda (form) (swap! a #'conj form) form)
                  [1 2 {:a 3} (list 4 [5])])
    (is (== (deref a)
            [[1 2 {:a 3} (list 4 [5])]
             1 2 {:a 3} (p::map-entry :a 3) :a 3
             (list 4 [5]) 4 [5] 5]))))

(test postwalk-order-test
  (is (== (let ((a (atom [])))
            (walk:postwalk (lambda (form) (swap! a #'conj form) form)
                           [1 2 {:a 3} (list 4 [5])])
            (deref  a))
          [1 2
             :a 3 (p::map-entry  :a 3) {:a 3}
             4 5 [5] (list 4 [5])
             [1 2 {:a 3} (list 4 [5])]])))

(test walk-mapentry-test
  (labels ((map-entry? (x) (instance? 'p::entry x))
           (f (e)
             (if (and (vector? e) (not (map-entry? e))) (->list e) e)))
    (let ( (coll [:html {:a ["b" 1]} ""]))
      (is (== (list :html {:a (list "b" 1)} "") (walk:postwalk #'f coll))))))

(test walk-maps
  (named-readtables:in-readtable persidastricl:syntax)
  (is (==  {":a" "1"}
           (walk:walk (lambda (e)
                        (apply #'p::map-entry (->list (map #'str e))))
                      (lambda (form)
                        form)
                      {:a 1}))))

;; (5am:run! :walk-tests)
