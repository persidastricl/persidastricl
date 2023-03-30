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
;;; test/atom.lisp
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :atom-tests
  :description "testing atom operations"
  :in master-suite)

(in-suite :atom-tests)

(test reset-test
  :description "test resetting an atom"
  (let ((a (atom {:a 1})))
    (is (= 1 (get (deref a) :a)))
    (reset! a {:a 2})
    (is (= 2 (get (deref a) :a)))))

(test swap-test
  :description "test swapping an atom's value"
  (let* ((a (atom {:a 1}))
         (prev (deref a)))
    (is (= 1 (get (deref a) :a)))
    (swap! a (fn (a) (update a :a #'inc)))
    (is (= 2 (get (deref a) :a)))
    (is (== prev {:a 1}))))

;; (5am:run! :atom-tests)
