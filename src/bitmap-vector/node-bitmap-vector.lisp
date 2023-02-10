;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   key-value-bitmap-vector.lisp
;;;
;;; -----

(in-package #:bitmap-vector)

;; -----
;;  mixin/marker class for bitmap-vectors that track sub-nodes
;;
;; -----

(defclass node-bitmap-vector (bitmap-vector) ())
