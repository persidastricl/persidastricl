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
  (:default-initargs :dmap (empty-transient-key-value-bitmap-vector) :nmap (empty-transient-node-bitmap-vector)))
