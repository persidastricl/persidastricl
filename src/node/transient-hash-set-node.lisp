;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-set-node
;;
;; -----

(defclass transient-hash-set-node (transient-node hash-set-node) ()
  (:default-initargs :dmap (empty-transient-bitmap-vector) :nmap (empty-transient-node-bitmap-vector)))
