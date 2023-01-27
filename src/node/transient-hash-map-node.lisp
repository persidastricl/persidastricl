;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-map.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; transient-hash-map-node
;;
;; -----

(defclass transient-hash-map-node (transient-node hash-map-node) ()
  (:default-initargs :dmap (EMPTY-TRANSIENT-KEY-VALUE-BITMAP-VECTOR) :nmap (EMPTY-TRANSIENT-NODE-BITMAP-VECTOR)))

;; -----
;; methods
;;
;; -----

(defmethod insert ((node transient-hash-map-node) position item)
  (with-slots (dmap) node
    (setf dmap (insert dmap position item)))
  node)

(defmethod insert ((node transient-hash-map-node) position (new-node transient-hash-map-node))
  (with-slots (nmap) node
    (setf nmap (insert nmap position new-node)))
  node)

(defmethod insert ((node transient-hash-map-node) position (new-node transient-hash-map-overflow-node))
  (with-slots (nmap) node
    (setf nmap (insert nmap position new-node)))
  node)

(defmethod update ((node transient-hash-map-node) position item)
  (with-slots (dmap) node
    (setf dmap (update dmap position item)))
  node)

(defmethod update ((node transient-hash-map-node) position (new-node transient-hash-map-node))
  (with-slots (nmap) node
    (setf nmap (update nmap position new-node)))
  node)

(defmethod update ((node transient-hash-map-node) position (new-node transient-hash-map-overflow-node))
  (with-slots (nmap) node
    (setf nmap (update nmap position new-node)))
  node)

(defmethod remove ((node transient-hash-map-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (setf dmap (remove dmap position))
        (setf nmap (remove nmap position))))
  node)

(defmethod lookup ((node transient-hash-map-node) key &optional (default nil))
  (let (hash (h:hash key))
    (get node key (list hash ))))
