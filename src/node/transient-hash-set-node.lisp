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
  (:default-initargs :dmap (EMPTY-TRANSIENT-BITMAP-VECTOR) :nmap (EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))
