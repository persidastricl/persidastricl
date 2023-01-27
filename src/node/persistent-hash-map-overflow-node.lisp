;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  persistent-hash-map-overflow-node
;;
;; -----

(define-immutable-class persistent-hash-map-overflow-node (hash-map-overflow-node) ())

(defmethod put ((node persistent-hash-map-overflow-node) entry context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (let ((key (e:key entry))
          (value (e:value entry)))
      (make-instance 'persistent-hash-map-overflow-node :hash (or hash (first context)) :data (->> data
                                                                                                (remove-if (lambda (e) (== (car e) key)))
                                                                                                (acons key value))))))

(defmethod del ((node persistent-hash-map-overflow-node) key context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (make-instance 'persistent-hash-map-overflow-node :hash (or hash (first context)) :data (remove-if (lambda (e) (== (car e) key)) data))))
