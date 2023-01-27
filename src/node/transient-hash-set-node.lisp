;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set-node.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; transient-hash-set-node
;;
;; -----

(defclass transient-hash-set-node (transient-node hash-set-node) ()
  (:default-initargs :dmap (EMPTY-TRANSIENT-BITMAP-VECTOR) :nmap (EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))

;; -----
;; methods
;;
;; -----

(defmethod insert ((node transient-hash-set-node) position item)
  (with-slots (dmap) node
    (setf dmap (insert dmap position item)))
  node)

(defmethod insert ((node transient-hash-set-node) position (new-node transient-hash-set-node))
  (with-slots (nmap) node
    (setf nmap (insert nmap position new-node)))
  node)

(defmethod insert ((node transient-hash-set-node) position (new-node transient-hash-set-overflow-node))
  (with-slots (nmap) node
    (setf nmap (insert nmap position new-node)))
  node)

(defmethod update ((node transient-hash-set-node) position item)
  (with-slots (dmap) node
    (setf dmap (update dmap position item)))
  node)

(defmethod update ((node transient-hash-set-node) position (new-node transient-hash-set-node))
  (with-slots (nmap) node
    (setf nmap (update nmap position new-node)))
  node)

(defmethod update ((node transient-hash-set-node) position (new-node transient-hash-set-overflow-node))
  (with-slots (nmap) node
    (setf nmap (update nmap position new-node)))
  node)

(defmethod remove ((node transient-hash-set-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (setf dmap (remove dmap position))
        (setf nmap (remove nmap position))))
  node)
