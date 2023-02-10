;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set-node.lisp
;;;
;;; -----

(in-package :node)

;; -----
;; transient-hash-set-node
;;
;; -----

(defclass transient-hash-set-node (transient-node hash-set-node) ()
  (:default-initargs :dmap (bv:EMPTY-TRANSIENT-BITMAP-VECTOR) :nmap (bv:EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))
