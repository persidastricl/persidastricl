;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-hash-map-node
;;
;; -----

(define-immutable-class persistent-hash-map-node (persistent-node hash-map-node) ()
  (:default-initargs :dmap (empty-persistent-key-value-bitmap-vector) :nmap (empty-persistent-node-bitmap-vector)))
