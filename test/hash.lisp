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
;;; test/hash.lisp
;;;
;;;  testing hashing
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :hash-tests
  :description "testing bit operations"
  :in master-suite)

(in-suite :hash-tests)

(test :hashing-things-tests
  :description "test hashing various things doesn't explode"
  (is (= 3399008174 (h:hash :keywords)))
  (is (= 3502813208 (h:hash "strings")))
  (is (= 2896334811 (h:hash 'symbols)))
  (is (= 2800891659 (h:hash 12345)))
  (is (= 1311568101 (h:hash #(1 2))))
  (is (= 240748949 (h:hash '(l i s t)))))

;; (5am:run! :hash-tests)
