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
;;;   test/persistent-hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :persistent-hash-map-tests
  :description "testing persistent hash map operations"
  :in master-suite)

(in-suite :persistent-hash-map-tests)

(named-readtables:in-readtable persidastricl:syntax)

(test persistent-hash-map-test
  (let ((m {:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep m 'persistent-hash-map))
    (is (= 4 (count m)))
    (is (equal :v1 (lookup m :k1)))
    (is (equal :v2 (lookup m :k2)))
    (is (equal :v3 (lookup m :k3)))
    (is (equal :v4 (lookup m :k4)))))

;;(5am:run! :persistent-hash-map-tests)
