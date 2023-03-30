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
;;; test/vector.lisp
;;;
;;;  testing adding/updating/removing items from a 'persistent' vector
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :vector-tests
  :description "testing immutable vector operations"
  :in master-suite)

(in-suite :vector-tests)

(test insert-prepend-test
  :description "test prepending an item onto a vector (returns a new vector; original unchanged)"
  (let* ((original #(0 1 2 3))
         (new (v:insert original 0 :a :b :c)))
    (is (equalp original #(0 1 2 3)))
    (is (equalp new #(:a :b :c 0 1 2 3)))))

(test insert-within-test
  :description "test inserting an item within a vector (returns a new vector; original unchanged)"
  (let* ((original #(0 1 2 3))
         (new (v:insert original 2 :a :b :c)))
    (is (equalp original #(0 1 2 3)))
    (is (equalp new #(0 1 :a :b :c 2 3)))))

(test insert-append-test
  :description "test appending an item onto vector (returns a new vector; original unchanged)"
  (let* ((original #(0 1 2 3))
         (new (v:insert original 4 :a :b :c)))
    (is (equalp original #(0 1 2 3)))
    (is (equalp new #(0 1 2 3 :a :b :c)))))

(test insert-out-of-bounds-test
  :description "test inserting an item with invalid position (out-of-bounds) (signals error)"
  (signals error (v:insert #(0 1 2 3) 5 :a :b :c)))


(test update-test
  :description "test updating various positions (original unchanged)"
  (let* ((original #(0 1 2 3 4)))
    (is (equalp #(:a :b :c 3 4) (v:update original 0 :a :b :c)))
    (is (equalp #(0 1 2 3 :a :b :c) (v:update original 4 :a :b :c)))
    (is (equalp #(0 1 :a :b :c) (v:update original 2 :a :b :c)))
    (signals error (v:update original 5 :a :b :c))
    (is (equalp #(0 1 2 3 4) original))))


(test delete-test
  :description "test deleting various positions (original unchanged)"
  (let* ((original #(0 1 2 3 4)))
    (is (equalp #(1 2 3 4) (v:delete original 0)))
    (is (equalp #(0 1 2 3) (v:delete original 4)))
    (is (equalp #(0 1 3 4) (v:delete original 2)))
    (is (equalp #(0 1 4) (v:delete original 2 2)))
    (signals error (v:delete original 5))
    (is (equalp #(0 1 2 3 4) original))))

;; (5am:run! :vector-tests)
