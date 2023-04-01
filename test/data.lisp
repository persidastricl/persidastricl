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
;;; test/data.lisp
;;;
;;;  testing data:diff
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :data-tests
  :description "testing data:diff with various data structures/data"
  :in master-suite)

(in-suite :data-tests)

(test diff-test
  (is (== [nil nil nil] (data:diff nil nil)))
  (is (== [1 2 nil] (data:diff 1 2)))
  (is (== [nil nil [1 2 3]] (data:diff [1 2 3] '(1 2 3))))
  (is (== [1 [:a :b] nil] (data:diff 1 [:a :b])))
  (is (== [{:a 1} :b nil] (data:diff  {:a 1} :b)))
  (is (== [:team #{:p1 :p2} nil] (data:diff  :team #{:p1 :p2})))
  (is (== [{0 :a} [:a] nil] (data:diff  {0 :a} [:a])))
  (is (== [nil [nil 2] [1]] (data:diff  [1] [1 2])))
  (is (== [nil nil [1 2]] (data:diff  [1 2] (into #() [1 2]))))
  (is (== [#{:a} #{:b} #{:c :d}] (data:diff #{:a :c :d} #{:b :c :d})))
  (is (== [nil nil {:a 1}] (data:diff  {:a 1} {:a 1})))
  (is (== [{:a #{2}} {:a #{4}} {:a #{3}}] (data:diff  {:a #{2 3}} {:a #{3 4}})))
  (is (== [#{1} #{3} #{2}] (data:diff  (set [1 2]) (set [2 3]))))
  (is (== [nil nil [1 2]] (data:diff  (into #() [1 2]) [1 2])))
  (is (== [{:a {:c [1]}} {:a {:c [0]}} {:a {:c [nil 2] :b 1}}] (data:diff  {:a {:b 1 :c [1 2]}} {:a {:b 1 :c [0 2]}})))
  (is (== [{:a nil} {:a 'false} {:b nil :c 'false}] (data:diff  {:a nil :b nil :c 'false} {:a 'false :b nil :c 'false}))))

;; (5am:run! :data-tests)
