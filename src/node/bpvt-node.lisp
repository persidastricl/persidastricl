;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   bpvt-node.lisp
;;;
;;; -----

(in-package #:node)

(defgeneric add-leaf-node (node leaf first-index-of-values))
(defgeneric get-leaf-node (node first-index-of-values))
(defgeneric remove-leaf-node (node first-index-of-values))

(defgeneric pop (node))

;; -----
;;  bpvt-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass bpvt-node (node) ())
