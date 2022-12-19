;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   vector.lisp
;;;
;;; utility functions for simple lisp vectors
;;;
;;; -----

(in-package #:vector)

(defun insert (v index item)
  (let* ((len (length v))
         (new (make-array (1+ len))))
    (loop for i from 0 below index
          do
             (setf (elt new i) (elt v i)))
    (setf (elt new index) item)
    (loop for i from index below len
          do
             (setf (elt new (1+ i)) (elt v i)))
    new))

(defun update (v index item)
  (let* ((len (length v))
         (new (make-array len)))
    (loop for i from 0 below index
          do
             (setf (elt new i) (elt v i)))
    (setf (elt new index) item)
    (loop for i from (1+  index) below len
          do
             (setf (elt new i) (elt v i)))
    new))

(defun delete (v index)
  (let* ((len (length v))
         (new (make-array (1- len))))
    (loop for i from 0 below index
          do
             (setf (elt new i) (elt v i)))
    (loop for i from (1+ index) below len
          do
             (setf (elt new (1- i)) (elt v i)))
    new))
