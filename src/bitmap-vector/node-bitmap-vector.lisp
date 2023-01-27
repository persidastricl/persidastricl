;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   key-value-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  mixin/marker class for bitmap-vectors that track nodes
;;
;; -----

(defclass node-bitmap-vector (bitmap-vector) ())
