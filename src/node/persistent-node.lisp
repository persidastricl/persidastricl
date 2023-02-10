;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  persistent-node
;;
;;  mixin class for any persistent nodes

(define-immutable-class persistent-node (hamt-node) ())

;; -----
;; methods
;;
;; -----

(defmethod insert ((node persistent-node) position item)
  (make-instance (type-of node) :dmap (bv:insert (:dmap node) position item) :nmap (:nmap node)))

(defmethod insert ((node persistent-node) position (new-node persistent-node))
  (make-instance (type-of node) :dmap (:dmap node) :nmap (bv:insert (:nmap node) position new-node)))

(defmethod insert ((node persistent-node) position (new-node persistent-overflow-node))
  (make-instance (type-of node) :dmap (:dmap node) :nmap (bv:insert (:nmap node) position new-node)))

(defmethod update ((node persistent-node) position item)
  (make-instance (type-of node) :dmap (bv:update (:dmap node) position item) :nmap (:nmap node)))

(defmethod update ((node persistent-node) position (new-node persistent-node))
  (make-instance (type-of node) :dmap (:dmap node) :nmap (bv:update (:nmap node) position new-node)))

(defmethod update ((node persistent-node) position (new-node persistent-overflow-node))
  (make-instance (type-of node) :dmap (:dmap node) :nmap (bv:update (:nmap node) position new-node)))

(defmethod remove ((node persistent-node) position)
  (with-slots (dmap nmap) node
    (if (bv:is-set dmap position)
        (make-instance (type-of node) :dmap (bv:remove dmap position) :nmap nmap)
        (make-instance (type-of node) :dmap dmap :nmap (bv:remove nmap position)))))
