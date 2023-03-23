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

(defun empty-transient-node-bitmap-vector ()
  (make-instance 'transient-node-bitmap-vector))
