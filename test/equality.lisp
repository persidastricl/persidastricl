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
;;; test/equality.lisp
;;;
;;; -----

(in-package #:persidastricl/test)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :equality-tests
  :description "testing scalar/object equality with `==`"
  :in master-suite)

(in-suite :equality-tests)

(test :scalars
  :description "test equality of atomic/scalar values"
  (is (== 1 1))
  (is (not (== 1 2)))
  (is (== #\a #\a))
  (is (not (== #\a #\b)))
  (is (== 1.0 1.000))
  (is (not (== 1.00 1.00001)))
  (is (== :a :a))
  (is (not (== :a :b)))
  (is (== 'a 'a))
  (is (not (== 'a 'b)))
  (is (== #'identity #'identity))
  (is (not (== #'identity #'expt))))

(test :strings
  :description "test equality of string values"
  (is (equalp "test" "Test")) ;; by default lisp is case insensitive!
  (is (== "test" "test"))
  (is (not (== "test" "Test"))))

(test :maps-and-tables
  :description "test equality of various types of maps and tables"
  (is (==  {:a 1 :b 2 :c {:d 4}}    {:a 1 :b 2 :c {:d 4}}))
  (is (== @{:a 1 :b 2 :c {:d 4}}  @{:a 1 :b 2 :c {:d 4}}))
  (is (== %{:a 1 :b 2 :c {:d 4}}  %{:a 1 :b 2 :c {:d 4}}))
  (is (==  {:a 1 :b 2 :c {:d 4}}   @{:a 1 :b 2 :c {:d 4}}))
  (is (==  {:a 1 :b 2 :c {:d 4}}   %{:a 1 :b 2 :c {:d 4}}))
  (is (== @{:a 1 :b 2 :c {:d 4}}  %{:a 1 :b 2 :c {:d 4}}))
  (is (not (==  {:a 1 :b 2 :c {:d 4}}    {:a 1 :b 2 :c {:d 3}})))
  (is (not (== @{:a 1 :b 2 :c {:d 4}}  @{:a 1 :b 2 :c {:d 3}})))
  (is (not (== %{:a 1 :b 2 :c {:d 4}}  %{:a 1 :b 2 :c {:d 3}})))
  (is (not (==  {:a 1 :b 2 :c {:d 4}}   @{:a 1 :b 2 :c {:d 3}})))
  (is (not (==  {:a 1 :b 2 :c {:d 4}}   %{:a 1 :b 2 :c {:d 3}})))
  (is (not (== @{:a 1 :b 2 :c {:d 4}}  %{:a 1 :b 2 :c {:d 3}}))))

(test :sequences-vectors-and-subvecs
  :description "test equality of various sequences, vectors, and subvecs"
  (is (== [1 2 3] [1 2 3]))
  (is (== [1 2 3] @[1 2 3]))
  (is (== [1 2 3] '(1 2 3)))
  (is (== [1 2 3] #(1 2 3)))
  (is (== @[1 2 3] '(1 2 3)))
  (is (== @[1 2 3] #(1 2 3)))
  (is (== (subvec [0 1 2 3 4] 1 4) [1 2 3]))
  (is (== (subvec [0 1 2 3 4] 1 4) @[1 2 3]))
  (is (== (subvec [0 1 2 3 4] 1 4) '(1 2 3)))
  (is (== (subvec [0 1 2 3 4] 1 4) #(1 2 3)))
  (is (== '(1 2 3) #(1 2 3)))
  (is (== #(1 2 3) '(1 2 3)))
  (is (== @[1 2 3] [1 2 3]))
  (is (== '(1 2 3) [1 2 3]))
  (is (== #(1 2 3) [1 2 3]))
  (is (== '(1 2 3) @[1 2 3]))
  (is (== #(1 2 3) @[1 2 3]))
  (is (== [1 2 3] (subvec [0 1 2 3 4] 1 4)))
  (is (== @[1 2 3] (subvec [0 1 2 3 4] 1 4)))
  (is (== '(1 2 3) (subvec [0 1 2 3 4] 1 4)))
  (is (== #(1 2 3) (subvec [0 1 2 3 4] 1 4))))

(test :equality-of-seqs
  :description "test equality of seqs of various sources"
  (is (== (range 9) '(0 1 2 3 4 5 6 7 8)))
  (is (== '(0 1 2 3 4 5 6 7 8) (range 9)))
  (is (== (range 9) [0 1 2 3 4 5 6 7 8]))
  (is (== [0 1 2 3 4 5 6 7 8] (range 9)))
  (is (not  (== (range 9) '(0 2 3 4 5 6 7 8))))
  (is (not  (== '(0 2 3 4 5 6 7 8) (range 9)))))

(test :equality-with-empty-vectors-arrays-and-lists
  :description "test equality of empty/nil data structurs with vectors and nil"
  (is (== #() []))
  (is (== [] #()))
  (is (not (== [] '())))
  (is (not (== [] nil)))
  (is (not (== #() '())))
  (is (not (== #() nil)))
  (is (not (== '() [])))
  (is (not (== nil [])))
  (is (not (== '() #())))
  (is (not (== nil #()))))

;; (5am:run! :sequences-vectors-and-subvecs)
