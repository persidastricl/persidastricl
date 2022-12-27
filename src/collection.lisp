;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   collection.lisp
;;;
;;; marker class for a collection of things
;;;
;;; -----

(in-package #:persidastricl)

(defclass collection () ())

(defgeneric contains? (collection item))
(defgeneric conj (collection &rest items))
(defgeneric disj (collection &rest items))
