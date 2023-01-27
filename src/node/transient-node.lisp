;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  transient-node
;;
;;  mixin class for any transient nodes

(defclass transient-node (hamt-node) ())
