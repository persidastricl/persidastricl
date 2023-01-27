;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  transient-hash-map-overflow-node
;;
;; -----

(defclass transient-hash-map-overflow-node (hash-map-overflow-node) ())

(defmethod put ((node transient-hash-map-overflow-node) entry context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (let ((key (e:key entry))
          (value (e:value entry)))
      (when-not hash (setf hash (first context)))
      (setf data (->> data
                   (remove-if (lambda (e) (== (car e) key)))
                   (acons key value)))))
  node)

(defmethod del ((node transient-hash-map-overflow-node) key context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (when-not hash (setf hash (first context)))
    (setf data (remove-if (lambda (e) (== (car e) key)) data)))
  node)
