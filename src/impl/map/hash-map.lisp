;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

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

(defmethod keys ((hm hash-map))
  (map #'key hm))

(defmethod vals ((hm hash-map))
  (map #'value hm))

(defmethod ->plist ((ht hash-table))
  (apply #'concatenate 'list
         (loop for v being each hash-values of ht using (hash-key k)
               collect (list k (if (or (map? v) (typep v 'hash-table)) (->plist v) v)))))

(defmethod ->alist ((ht hash-table))
  (->list
   (map
    (lambda (e)
      (let ((k (first e))
            (v (second e)))
        (cond
          ((or (map? v) (typep v 'hash-table)) (cons k (->alist v)))
          (:otherwise (cons k v)))))
    ht)))

(defmethod ->plist ((hm hash-map))
  (->list
   (mapcat
    (lambda (e)
      (let ((k (key e))
            (v (value e)))
        (list k (if (or (map? v) (typep v 'hash-table)) (->plist v) v))))
    hm)))

(defmethod ->alist ((hm hash-map))
  (->list
   (map
    (lambda (e)
      (let ((k (key e))
            (v (value e)))
        (cond
          ((or (map? v) (typep v 'hash-table)) (cons k (->alist v)))
          (:otherwise (cons k v)))))
    hm)))

(defmethod with-meta ((object hamt) (meta (eql nil))) object)

(defun select-keys (map keyseq)
  (labels ((select-keys* (m keys)
             (if keys
                 (let ((k (head keys)))
                   (select-keys*
                    (if-let ((v (get map k)))
                      (assoc m k v)
                      m)
                    (tail keys)))
                 m)))
    (with-meta (select-keys* (empty map) (seq keyseq)) (meta map))))

(defmacro with-funcallable-map ((symbol definition) &body body)
  `(let ((,symbol ,definition))
     (labels ((,symbol (k &optional (default nil))
                (lookup ,symbol k default)))
       ,@body)))

(defmethod == ((ht1 hash-map) ht2)
  (when (or (instance? 'hash-table ht2) (instance? 'hash-map ht2))
    (or (eq ht1 ht2)
        (let ((ks1 (set (keys ht1))))
          (and
           (== ks1 (set (keys ht2)))
           (every
            (lambda (k)
              (== (get ht1 k) (get ht2 k)))
            (->list ks1)))))))

(defmethod == (ht1 (ht2 hash-map))
  (== ht2 ht1))

(defmethod compare ((hm1 hash-map) (hm2 hash-map))
  (compare (seq hm1) (seq hm2)))
