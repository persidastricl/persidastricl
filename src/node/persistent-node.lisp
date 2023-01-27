;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  persistent-node
;;
;;  mixin class for any persistent nodes

(define-immutable-class persistent-node (hamt-node) ())
