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
      (entry sequence)
      (cl:cons (cl:map 'list (lambda (c) (apply #'map-entry c)) sequence))
      (t (cl:map 'list (lambda (kv) (apply #'map-entry kv)) (->list (partition-all sequence 2)))))))

(defclass hash-map (hamt associable)
  ((root :initarg :root :reader root)))

(defun map? (x)
  (typep x 'hash-map))

(defmethod lookup ((hm hash-map) key &optional (default nil))
  (with-slots (root) hm
    (let ((hash (h:hash key)))
      (loc root key :hash hash :depth 0 :default default))))

(defmethod get ((hm hash-map) key &optional (default nil))
  (lookup hm key default))

(defmethod ->vector ((hm hash-map))
  (cl:map 'cl:vector #'->vector (->list hm)))

(defmethod ->vec ((hm hash-map))
  (into (persistent-vector) (map #'->vec (->list hm))))

(defmethod ->array ((hm hash-map))
  (->vector hm))

(defmethod ->list ((hm hash-map))
  (into '() (seq hm)))

(defmethod ->keys ((hm hash-map))
  (map #'key hm))

(defmethod ->vals ((hm hash-map))
  (map #'value hm))

(defmethod ->plist ((hm hash-map))
  (mapcat #'->list hm))

(defmethod ->alist ((hm hash-map))
  (map #'->cons hm))

(defmethod with-meta ((object hamt) (meta hash-map))
  (with-slots (root) object
    (make-instance (type-of object) :root root :meta meta)))

(defun select-keys (map keyseq)
  (labels ((select-keys* (m keys)
             (if keys
                 (let ((k (first keys)))
                   (select-keys*
                    (if-let ((v (get map k)))
                      (assoc m k v)
                      m)
                    (tail keys)))
                 (with-meta m (meta map)))))
    (select-keys* (empty map) keyseq)))

(defmacro with-funcallable-map ((symbol definition) &body body)
  `(let ((,symbol ,definition))
     (labels ((,symbol (k &optional (default nil))
                (lookup ,symbol k default)))
       ,@body)))
