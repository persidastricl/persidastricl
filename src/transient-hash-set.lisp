;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set.lisp
;;;
;;; transient hash set
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

(defmethod update ((node transient-hash-set-node) position item)
  (with-slots (dmap) node
    (setf dmap (update dmap position item)))
  node)

(defmethod update ((node transient-hash-set-node) position (new-node transient-hash-set-node))
  (with-slots (nmap) node
    (setf nmap (update nmap position new-node)))
  node)

(defmethod remove ((node transient-hash-set-node) position)
  (with-slots (dmap nmap) node
    (if (is-set dmap position)
        (setf dmap (remove dmap position))
        (setf nmap (remove nmap position))))
  node)


;; -----
;; transient-hash-set
;;
;;  an implementation of a transient/imperative hashed-array-mapped-trie (hamt)
;; -----

(defclass transient-hash-set (hash-set metadata collection)
  ((root :type 'transient-hash-set-node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root (make-instance 'transient-hash-set-node) :meta nil))

(defmethod conj ((ths transient-hash-set) &rest items)
  (if (null items)
      ths
      (with-slots (root) ths
        (labels ((conj* (node item)
                   (if (contains? node item)
                       node
                       (put node item (list (h:hash item) 0)))))
          (setf root (reduce #'conj* items :initial-value root))
          ths))))

(defmethod disj ((ths transient-hash-set) &rest items)
  (if (null items)
      ths
      (with-slots (root) ths
        (labels ((disj* (node item)
                   (if (contains? node item)
                       (del node item (list (h:hash item) 0))
                       node)))
          (setf root (reduce #'disj* items :initial-value root))
          ths))))
