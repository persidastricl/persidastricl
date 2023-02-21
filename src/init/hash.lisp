;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash.lisp
;;;
;;; -----

(in-package #:hash)

(defclass hasher ()
  ((size :reader :size :initarg :size)
   (fn :reader :fn :initarg :fn)))

(defgeneric do-hash (fn object &rest args)
  (:method (fn (object t) &rest args) (apply fn object args)))

;; NOTE:  this needs more thought; we need to be able to take a collection/sequence and hash it as a mixin of the 'items' in the coll/seq
;; ex   (defmethod ??????? ((obj persistent-hash-map) &rest args)
;;        (let ((hash))
;;          (doseq (entry obj)
;;            (setf hash (mixin hash (key entry) ...))
;;            (setf hash (mixin hash (value entry) ...)))
;;          hash))

(defvar murmur32 (make-instance 'hasher
                                :size 32
                                :fn (lambda (obj &rest args)
                                      (let ((cl-murmurhash:*hash-size* 32))
                                        (apply #'do-hash #'cl-murmurhash:murmurhash obj args)))))

(defvar murmur128 (make-instance 'hasher
                                 :size 128
                                 :fn (lambda (obj &rest args)
                                       (let ((cl-murmurhash:*hash-size* 128))
                                         (apply #'do-hash #'cl-murmurhash:murmurhash obj args)))))

(defvar sip-hash64 (make-instance 'hasher
                                  :size 64
                                  :fn (lambda (obj &rest args)
                                        (apply #'do-hash #'sip-hash:hash-64-4-8 (sb-ext:string-to-octets (s:str obj)) args
                                               ))))


(defvar sip-hash128 (make-instance 'hasher
                                   :size 128
                                   :fn (lambda (obj &rest args)
                                         (multiple-value-bind (lo hi) (apply #'do-hash #'sip-hash:hash-128-4-8 (sb-ext:string-to-octets (s:str obj)) args)
                                           (+ (ash hi 64) lo)))))

(defparameter *default-hasher* murmur32)

(defun size () (:size *default-hasher*))

(defgeneric hash (obj &optional hasher &rest args)
  (:method ((obj t) &optional (hasher *default-hasher*) &rest args) (apply (:fn hasher) obj args)))


(defvar constant5bit (make-instance 'hasher
                                    :size 5
                                    :fn (lambda (obj &rest args)
                                          #b00001)))
