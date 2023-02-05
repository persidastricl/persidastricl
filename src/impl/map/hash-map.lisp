;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-map.lisp
;;;
;;; mixin class for persistent/transient -hash-map classes
;;;
;;; -----

(in-package #:persidastricl)

(defclass hash-map (hamt associable) ())

(defmethod lookup ((hm hash-map) key &optional (default nil))
  (with-slots (root bit-size) hm
    (let ((hash (h:hash key)))
      (get-it root key (list hash 0 bit-size default)))))

(defmethod get ((hm hash-map) key &optional (default nil))
  (lookup hm key default))

(defmethod ->vector ((hm hash-map))
  (map 'vector #'e:->vec (seq hm)))

(defmethod ->vec ((hm hash-map))
  (->vector hm))

(defmethod ->array ((hm hash-map))
  (->vector hm))

(defmethod ->list ((hm hash-map))
  (flatten (map 'list #'e:->list (seq hm))))

(defmethod ->plist ((hm hash-map))
  (->list hm))

(defmethod ->alist ((hm hash-map))
  (map 'list #'e:->cons (seq hm)))

(defmethod with-meta ((object hamt) (meta hash-map))
  (with-slots (root bit-size count) object
    (make-instance (type-of object) :root root :bit-size bit-size :count count :meta meta)))
