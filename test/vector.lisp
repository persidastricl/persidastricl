;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/vector.lisp
;;;
;;;  testing adding/updating/removing items from a 'persistent' vector
;;;
;;; -----

(in-package #:vector-test)

(def-suite vector-tests
  :description ""
  :in master-suite)

(in-suite vector-tests)

(test insert-prepend-test
  :description "test prepending an item onto a vector (returns a new vector; original unchanged)"
  (let* ((original #(0 1 2 3))
         (new (v:insert original 0 :new)))
    (is (equalp original #(0 1 2 3)))
    (is (equalp new #(:new 0 1 2 3)))))

(test insert-within-test
  :description "test inserting an item within a vector (returns a new vector; original unchanged)"
  (let* ((original #(0 1 2 3))
         (new (v:insert original 2 :new)))
    (is (equalp original #(0 1 2 3)))
    (is (equalp new #(0 1 :new 2 3)))))

(test insert-append-test
  :description "test appending an item onto vector (returns a new vector; original unchanged)"
  (let* ((original #(0 1 2 3))
         (new (v:insert original 4 :new)))
    (is (equalp original #(0 1 2 3)))
    (is (equalp new #(0 1 2 3 :new)))))

(test insert-out-of-bounds-test
  :description "test inserting an item with invalid position (out-of-bounds) (signals error)"
  (signals error (v:insert #(0 1 2 3) 5 :new)))


(test update-test
  :description "test updating various positions (original unchanged)"
  (let* ((original #(0 1 2 3 4)))
    (is (equalp #(:updated 1 2 3 4) (v:update original 0 :updated)))
    (is (equalp #(0 1 2 3 :updated) (v:update original 4 :updated)))
    (is (equalp #(0 1 :updated 3 4) (v:update original 2 :updated)))
    (signals error (v:update original 5 :updated))
    (is (equalp #(0 1 2 3 4) original))))


(test delete-test
  :description "test deleting various positions (original unchanged)"
  (let* ((original #(0 1 2 3 4)))
    (is (equalp #(1 2 3 4) (v:delete original 0)))
    (is (equalp #(0 1 2 3) (v:delete original 4)))
    (is (equalp #(0 1 3 4) (v:delete original 2)))
    (signals error (v:delete original 5))
    (is (equalp #(0 1 2 3 4) original)))
  )
