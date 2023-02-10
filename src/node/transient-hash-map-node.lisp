;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map.lisp
;;;
;;; -----

(in-package :node)

;; -----
;; transient-hash-map-node
;;
;; -----

(defclass transient-hash-map-node (transient-node hash-map-node) ()
  (:default-initargs :dmap (bv:EMPTY-TRANSIENT-KEY-VALUE-BITMAP-VECTOR) :nmap (bv:EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))
