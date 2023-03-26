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
;;;   test/transient-hash-set.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :transient-hash-set-tests
  :description "testing transient hash set operations"
  :in master-suite)

(in-suite :transient-hash-set-tests)

(named-readtables:in-readtable persidastricl:syntax)

(test transient-hash-set-test
  (let ((s @#{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep s 'transient-hash-set))
    (is (= 8 (count s)))
    (is (contains? s :k1))))

;;(5am:run! :transient-hash-set-tests)
