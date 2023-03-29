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
;;; test/syntax.lisp
;;;
;;;  testing syntax issues
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :syntax-tests
  :description "testing bit operations"
  :in master-suite)

(in-suite :syntax-tests)

(test reading-nil-test
  :description "test syntax with nil elements"
  (is #{nil})
  (is @#{nil})
  (is {:a nil})
  (is @{:a nil})
  (is {nil :a})
  (is @{nil :a})
  (is [nil])
  (is @[nil])
  (is %{:a nil})
  (is %{nil :a}))

;; (5am:run! :syntax-tests)

(comment
  (identity #{nil})
  (identity @#{nil})
  (identity {:a nil})
  (identity @{:a nil})
  (identity {nil :a})
  (identity @{nil :a})
  (identity [nil])
  (identity @[nil])
  (identity %{:a nil})
  (identity %{nil :a}))
