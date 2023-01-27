;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hamt.lisp
;;;
;;; base class for persistent/transient hashed array mapped trie classes (sets, maps, etc)
;;;
;;; -----

(in-package #:persidastricl)

(defclass hamt (metadata counted seqable)
  ((bit-size :initarg :bit-size :reader :bit-size :documentation "bit partition size of hash for hamt bitmap (default is 5)")
   (root :initarg :root :reader :root :documentation "root node of this hashed array mapped trie"))
  (:default-initargs :bit-size 5))

(defmethod count ((obj hamt))
  (:count obj))
