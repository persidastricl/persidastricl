;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   bpvt-node.lisp
;;;
;;; -----

(in-package #:node)

(defgeneric add-leaf-node (node leaf first-index-of-values))

;; -----
;;  bpvt-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass bpvt-node (node) ())
