;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-node-bitmap-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-node-bitmap-vector object
;;
;; -----

(define-immutable-class persistent-node-bitmap-vector (persistent-bitmap-vector) ())

(defun empty-persistent-node-bitmap-vector ()
  (make-instance 'persistent-node-bitmap-vector))
