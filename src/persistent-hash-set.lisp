;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set.lisp
;;;
;;; persistent hash set
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

(defmethod update ((node persistent-hash-set-node) position item)
  (make-instance 'persistent-hash-set-node :dmap (update (:dmap node) position item) :nmap (:nmap node)))

(defmethod update ((node persistent-hash-set-node) position (new-node persistent-hash-set-node))
  (make-instance 'persistent-hash-set-node :dmap (:dmap node) :nmap (update (:nmap node) position new-node)))

(defmethod remove ((node persistent-hash-set-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (make-instance 'persistent-hash-set-node :dmap (remove dmap position) :nmap nmap)
        (make-instance 'persistent-hash-set-node :dmap dmap :nmap (remove nmap position)))))

;; -----
;; persistent-hash-set
;;
;;  an implementation of a persistent/immutable hashed-array-mapped-trie (hamt)
;; -----

(define-immutable-class persistent-hash-set (hash-set metadata collection)
  ((root :type 'persistent-hash-set-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'persistent-hash-set-node) :meta nil))

(defmethod conj ((phs persistent-hash-set) &rest items)
  (if (null items)
      phs
      (labels ((conj* (node item)
                 (if (contains? node item)
                     node
                     (put node item (list (h:hash item) 0)))))
        (make-instance 'persistent-hash-set
                       :root (reduce #'conj* items :initial-value (:root phs))
                       :meta (:meta phs)))))


(defmethod disj ((phs persistent-hash-set) &rest items)
  (if (null items)
      phs
      (labels ((disj* (node item)
                 (if (contains? node item)
                     (del node item (list (h:hash item) 0))
                     node)))
        (make-instance 'persistent-hash-set
                       :root (reduce #'disj* items :initial-value (:root phs))
                       :meta (:meta phs)))))
