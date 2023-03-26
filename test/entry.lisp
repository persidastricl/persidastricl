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
;;; test/entry.lisp
;;;
;;;  testing making/using map entries (vector tuples)
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :entry-tests
  :description "testing map entries"
  :in master-suite)

(in-suite :entry-tests)

(defun proper-entry-p (entry k v)
  (is (equalp k (key entry)))
  (is (equalp v (value entry))))

(test entry-creation-tests
  :description "test the making of a map entry 'tuple'"
  (is (proper-entry-p (map-entry :k  :v)  :k  :v))
  (is (proper-entry-p (map-entry "k" "v") "k" "v"))
  (is (proper-entry-p (map-entry 'k  'v)  'k  'v))
  (is (proper-entry-p (map-entry  1  2)    1   2)))

(test key-tests
  :description "test pulling they key out of map entries"
  (is (eq :k (key (map-entry :k :v)))))

(test value-tests
  :description "test pulling the value out of map entries"
  (is (eq :v (value (map-entry :k :v)))))
