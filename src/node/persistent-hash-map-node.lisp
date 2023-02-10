;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map-node.lisp
;;;
;;; -----

(in-package :node)

;; -----
;; persistent-hash-map-node
;;
;; -----

(define-immutable-class persistent-hash-map-node (persistent-node hash-map-node) ()
  (:default-initargs :dmap (bv:EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR) :nmap (bv:EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))
