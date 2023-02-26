;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; bpvt.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass bpvt (counted collection seqable)
  ((root :initarg :root :reader root)
   (tail :initarg :tail :reader tail)
   (tail-offset :initarg :tail-offset :reader tail-offset)))

(defmethod cl-murmurhash:murmurhash ((object bpvt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))
