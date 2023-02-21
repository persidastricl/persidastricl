;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  transient-node
;;
;;  mixin class for any transient nodes

(defclass transient-node (hamt-node) ())

;; -----
;; methods
;;
;; -----

(defmethod ins ((node transient-node) position item)
  (with-slots (dmap) node
    (setf dmap (ins dmap position item)))
  node)

(defmethod ins ((node transient-node) position (new-node transient-node))
  (with-slots (nmap) node
    (setf nmap (ins nmap position new-node)))
  node)

(defmethod ins ((node transient-node) position (new-node transient-overflow-node))
  (with-slots (nmap) node
    (setf nmap (ins nmap position new-node)))
  node)

(defmethod upd ((node transient-node) position item)
  (with-slots (dmap) node
    (setf dmap (upd dmap position item)))
  node)

(defmethod upd ((node transient-node) position (new-node transient-node))
  (with-slots (nmap) node
    (setf nmap (upd nmap position new-node)))
  node)

(defmethod upd ((node transient-node) position (new-node transient-overflow-node))
  (with-slots (nmap) node
    (setf nmap (upd nmap position new-node)))
  node)

(defmethod del ((node transient-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (setf dmap (del dmap position))
        (setf nmap (del nmap position))))
  node)
