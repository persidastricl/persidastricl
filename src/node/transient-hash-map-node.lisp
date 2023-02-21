;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-map-node
;;
;; -----

(defclass transient-hash-map-node (transient-node hash-map-node) ()
  (:default-initargs :dmap (EMPTY-TRANSIENT-KEY-VALUE-BITMAP-VECTOR) :nmap (EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))
