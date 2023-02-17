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

(defmethod lookup ((hm hash-map) key &optional (default nil))
  (with-slots (root) hm
    (let ((hash (h:hash key)))
      (n:get root key (list hash 0 default)))))

(defmethod get ((hm hash-map) key &optional (default nil))
  (lookup hm key default))

(defmethod ->vector ((hm hash-map))
  (map 'cl:vector #'e:->vec (seq hm)))

(defmethod ->vec ((hm hash-map))
  (->vector hm))

(defmethod ->array ((hm hash-map))
  (->vector hm))

(defmethod ->list ((hm hash-map))
  (reduce
   (lambda (l e)
     (list* (e:key e) (e:value e) l))
   (seq hm)
   :initial-value '()))

(defmethod ->plist ((hm hash-map))
  (->list hm))

(defmethod ->alist ((hm hash-map))
  (map 'list #'e:->cons (seq hm)))

(defmethod count ((obj hash-map))
  (count (->alist obj)))

(defmethod with-meta ((object hamt) (meta hash-map))
  (with-slots (root) object
    (make-instance (type-of object) :root root :meta meta)))
