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

(define-immutable-class persistent-hash-map-overflow-node (persistent-overflow-node hash-map-overflow-node) ())

(defmethod add ((node persistent-hash-map-overflow-node) entry &key hash &allow-other-keys)
  (when (:hash node) (assert (eq (:hash node) hash)))
  (let ((key (key entry))
        (value (value entry)))
    (make-instance 'persistent-hash-map-overflow-node :hash (or (:hash node) hash) :data (->> (:data node)
                                                                                              (remove-if (lambda (e) (== (car e) key)))
                                                                                              (acons key value)))))

(defmethod remove ((node persistent-hash-map-overflow-node) key &key hash &allow-other-keys)
  (when (:hash node) (assert (eq (:hash node) hash)))
  (make-instance 'persistent-hash-map-overflow-node :hash (or (:hash node) hash) :data (remove-if (lambda (e) (== (car e) key)) (:data node))))
