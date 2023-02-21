;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-hash-set-node
;;
;; -----

(define-immutable-class persistent-hash-set-node (persistent-node hash-set-node) ()
  (:default-initargs :dmap (EMPTY-PERSISTENT-BITMAP-VECTOR) :nmap (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))
