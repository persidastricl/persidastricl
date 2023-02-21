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

(defmethod loc ((node hash-map-overflow-node) key &key hash (default nil) &allow-other-keys)
  (when (:hash node) (assert (= (:hash node) hash)))
  (if-let ((target (cl:assoc key (:data node) :test #'==)))
    (rest target)
    default))
