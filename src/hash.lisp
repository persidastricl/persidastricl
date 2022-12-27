;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash.lisp
;;;
;;; hashing function
;;;
;;;  currently it is murmur hashing but this could change (ie. xxhash)
;;;
;;; -----

(in-package #:hash)

(defun hash (item)
  "given an item of any type return its 32-bit hash"
  (cl-murmurhash:murmurhash item))
