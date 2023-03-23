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
  (:default-initargs :dmap (empty-persistent-bitmap-vector) :nmap (empty-persistent-node-bitmap-vector)))
