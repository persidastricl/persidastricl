;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  persistent-node
;;
;;  mixin class for any persistent nodes

(define-immutable-class persistent-node (hamt-node) ())

;; -----
;; methods
;;
;; -----

(defmethod ins ((node persistent-node) position item)
  (make-instance (type-of node) :dmap (ins (dmap node) position item) :nmap (nmap node)))

(defmethod ins ((node persistent-node) position (new-node persistent-node))
  (make-instance (type-of node) :dmap (dmap node) :nmap (ins (nmap node) position new-node)))

(defmethod ins ((node persistent-node) position (new-node persistent-overflow-node))
  (make-instance (type-of node) :dmap (dmap node) :nmap (ins (nmap node) position new-node)))

(defmethod upd ((node persistent-node) position item)
  (make-instance (type-of node) :dmap (upd (dmap node) position item) :nmap (nmap node)))

(defmethod upd ((node persistent-node) position (new-node persistent-node))
  (make-instance (type-of node) :dmap (dmap node) :nmap (upd (nmap node) position new-node)))

(defmethod upd ((node persistent-node) position (new-node persistent-overflow-node))
  (make-instance (type-of node) :dmap (dmap node) :nmap (upd (nmap node) position new-node)))

(defmethod del ((node persistent-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (make-instance (type-of node) :dmap (del dmap position) :nmap nmap)
        (make-instance (type-of node) :dmap dmap :nmap (del nmap position)))))
