;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hamt.lisp
;;;
;;; base class for persistent/transient hashed array mapped trie classes (sets, maps, etc)
;;;
;;; -----

(in-package #:persidastricl)

(defvar *print-hamt-items* 10)

(defclass hamt (metadata seqable) ())

(defmethod cl-murmurhash:murmurhash ((object hamt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))

(defmethod count ((object hamt))
  (count (seq object)))

(defmethod first ((object hamt))
  (first (seq object)))

(defmethod rest ((object hamt))
  (rest (seq object)))

(defmethod next ((object hamt))
  (next (seq object)))

(defmethod head ((object hamt))
  (head (seq object)))

(defmethod tail ((object hamt))
  (tail (seq object)))
