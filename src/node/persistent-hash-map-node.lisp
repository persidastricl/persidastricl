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
  (:default-initargs :dmap (EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR) :nmap (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))
