;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set-node.lisp
;;;
;;; -----

(in-package :node)

;; -----
;; persistent-hash-set-node
;;
;; -----

(define-immutable-class persistent-hash-set-node (persistent-node hash-set-node) ()
  (:default-initargs :dmap (bv:EMPTY-PERSISTENT-BITMAP-VECTOR) :nmap (bv:EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))
