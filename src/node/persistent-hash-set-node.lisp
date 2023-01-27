;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set-node.lisp
;;;
;;; -----

(in-package :persidastricl)

;; -----
;; persistent-hash-set-node
;;
;; -----

(define-immutable-class persistent-hash-set-node (persistent-node hash-set-node) ()
  (:default-initargs :dmap (EMPTY-PERSISTENT-BITMAP-VECTOR) :nmap (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))

;; -----
;; methods
;;
;; -----

(defmethod insert ((node persistent-hash-set-node) position item)
  (make-instance 'persistent-hash-set-node :dmap (insert (:dmap node) position item) :nmap (:nmap node)))

(defmethod insert ((node persistent-hash-set-node) position (new-node persistent-hash-set-node))
  (make-instance 'persistent-hash-set-node :dmap (:dmap node) :nmap (insert (:nmap node) position new-node)))

(defmethod insert ((node persistent-hash-set-node) position (new-node persistent-hash-set-overflow-node))
  (make-instance 'persistent-hash-set-node :dmap (:dmap node) :nmap (insert (:nmap node) position new-node)))

(defmethod update ((node persistent-hash-set-node) position item)
  (make-instance 'persistent-hash-set-node :dmap (update (:dmap node) position item) :nmap (:nmap node)))

(defmethod update ((node persistent-hash-set-node) position (new-node persistent-hash-set-node))
  (make-instance 'persistent-hash-set-node :dmap (:dmap node) :nmap (update (:nmap node) position new-node)))

(defmethod update ((node persistent-hash-set-node) position (new-node persistent-hash-set-overflow-node))
  (make-instance 'persistent-hash-set-node :dmap (:dmap node) :nmap (update (:nmap node) position new-node)))

(defmethod remove ((node persistent-hash-set-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (make-instance 'persistent-hash-set-node :dmap (remove dmap position) :nmap nmap)
        (make-instance 'persistent-hash-set-node :dmap dmap :nmap (remove nmap position)))))
