;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-node-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-node-bitmap-vector object
;;
;; -----

(defclass transient-node-bitmap-vector (transient-bitmap-vector) ())

(defun EMPTY-TRANSIENT-NODE-BITMAP-VECTOR ()
  (make-instance 'transient-node-bitmap-vector))
