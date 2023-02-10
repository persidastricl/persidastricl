;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-node-bitmap-vector.lisp
;;;
;;; -----

(in-package #:bitmap-vector)

;; -----
;; persistent-node-bitmap-vector object
;;
;; -----

(define-immutable-class persistent-node-bitmap-vector (persistent-bitmap-vector) ())

(defun EMPTY-PERSISTENT-NODE-BITMAP-VECTOR ()
  (make-instance 'persistent-node-bitmap-vector))
