;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  transient-node
;;
;;  mixin class for any transient nodes

(defclass transient-node (hamt-node) ())

;; -----
;; methods
;;
;; -----

(defmethod insert ((node transient-node) position item)
  (with-slots (dmap) node
    (setf dmap (bv:insert dmap position item)))
  node)

(defmethod insert ((node transient-node) position (new-node transient-node))
  (with-slots (nmap) node
    (setf nmap (bv:insert nmap position new-node)))
  node)

(defmethod insert ((node transient-node) position (new-node overflow-node))
  (with-slots (nmap) node
    (setf nmap (bv:insert nmap position new-node)))
  node)

(defmethod update ((node transient-node) position item)
  (with-slots (dmap) node
    (setf dmap (bv:update dmap position item)))
  node)

(defmethod update ((node transient-node) position (new-node transient-node))
  (with-slots (nmap) node
    (setf nmap (bv:update nmap position new-node)))
  node)

(defmethod update ((node transient-node) position (new-node overflow-node))
  (with-slots (nmap) node
    (setf nmap (bv:update nmap position new-node)))
  node)

(defmethod remove ((node transient-node) position)
  (with-slots (dmap nmap) node
    (if (bv:is-set dmap position)
        (setf dmap (bv:remove dmap position))
        (setf nmap (bv:remove nmap position))))
  node)
