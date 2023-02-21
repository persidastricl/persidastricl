;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-map.lisp
;;;
;;; mixin class for persistent/transient -hash-map classes
;;;
;;; -----

(in-package #:persidastricl)

(defun ->entries (sequence)
  (let ((sequence (->list sequence)))
    (typecase (first sequence)
      (e:entry sequence)
      (cl:cons (map 'list (lambda (c) (apply #'e:map-entry c)) sequence))
      (t (map 'list (lambda (kv) (apply #'e:map-entry kv)) (->list (partition-all sequence 2)))))))

(defclass hash-map (hamt associable)
  ((root :initarg :root :reader :root)))

(defun map? (x)
  (typep x 'hash-map))

(defmethod lookup ((hm hash-map) key &optional (default nil))
  (with-slots (root) hm
    (let ((hash (h:hash key)))
      (loc root key :hash hash :depth 0 :default default))))

(defmethod get ((hm hash-map) key &optional (default nil))
  (lookup hm key default))

(defmethod ->vector ((hm hash-map))
  (map 'cl:vector #'e:->vec (->list hm)))

(defmethod ->vec ((hm hash-map))
  (->vector hm))

(defmethod ->array ((hm hash-map))
  (->vector hm))

(defmethod ->list ((hm hash-map))
  (into '() (seq hm)))

(defmethod ->keys ((hm hash-map))
  (lmap #'e:key hm))

(defmethod ->vals ((hm hash-map))
  (lmap #'e:value hm))

(defmethod ->plist ((hm hash-map))
  (mapcat #'e:->list hm))

(defmethod ->alist ((hm hash-map))
  (lmap #'e:->cons hm))

(defmethod count ((obj hash-map))
  (count (->list obj)))

(defmethod with-meta ((object hamt) (meta hash-map))
  (with-slots (root) object
    (make-instance (type-of object) :root root :meta meta)))
