;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hamt.lisp
;;;
;;; base class for persistent/transient hashed array mapped trie classes (sets, maps, etc)
;;;
;;; -----

(in-package #:persidastricl)

(defclass hamt (metadata seqable) ())

(defmethod cl-murmurhash:murmurhash ((object hamt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))
