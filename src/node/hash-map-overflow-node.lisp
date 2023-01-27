;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-map-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  hash-map-overflow-node
;;
;; -----

(defclass hash-map-overflow-node (overflow-node) ())

(defmethod at-index ((node hash-map-overflow-node) index)
  (let ((e (elt (:data node) index)))
    (e:map-entry (first e) (rest e))))

(defmethod single-remaining-data ((node hash-map-overflow-node))
  (let ((target (first (:data node))))
    (e:map-entry (first target) (rest target))))

(defmethod get ((node hash-map-overflow-node) key context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (if-let (target (cl:assoc key data :test #'==))
      (rest target)
      (last context))))
