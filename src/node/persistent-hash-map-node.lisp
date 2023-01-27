;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-map-node.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; persistent-hash-map-node
;;
;; -----

(define-immutable-class persistent-hash-map-node (persistent-node hash-map-node) ()
  (:default-initargs :dmap (EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR) :nmap (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))

;; -----
;; methods
;;
;; -----

(defmethod insert ((node persistent-hash-map-node) position item)
  (make-instance 'persistent-hash-map-node :dmap (insert (:dmap node) position item) :nmap (:nmap node)))

(defmethod insert ((node persistent-hash-map-node) position (new-node persistent-hash-map-node))
  (make-instance 'persistent-hash-map-node :dmap (:dmap node) :nmap (insert (:nmap node) position new-node)))

(defmethod insert ((node persistent-hash-map-node) position (new-node persistent-hash-map-overflow-node))
  (make-instance 'persistent-hash-map-node :dmap (:dmap node) :nmap (insert (:nmap node) position new-node)))

(defmethod update ((node persistent-hash-map-node) position item)
  (make-instance 'persistent-hash-map-node :dmap (update (:dmap node) position item) :nmap (:nmap node)))

(defmethod update ((node persistent-hash-map-node) position (new-node persistent-hash-map-node))
  (make-instance 'persistent-hash-map-node :dmap (:dmap node) :nmap (update (:nmap node) position new-node)))

(defmethod update ((node persistent-hash-map-node) position (new-node persistent-hash-map-overflow-node))
  (make-instance 'persistent-hash-map-node :dmap (:dmap node) :nmap (update (:nmap node) position new-node)))

(defmethod remove ((node persistent-hash-map-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (make-instance 'persistent-hash-map-node :dmap (remove dmap position) :nmap nmap)
        (make-instance 'persistent-hash-map-node :dmap dmap :nmap (remove nmap position)))))
