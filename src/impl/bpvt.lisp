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

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; bpvt.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defvar *print-bpvt-items* 100)

(defclass bpvt (metadata counted collection seqable)
  ((root :initarg :root :reader root)
   (tail-end :initarg :tail-end :reader tail-end)
   (tail-offset :initarg :tail-offset :reader tail-offset)))

(defmethod cl-murmurhash:murmurhash ((object bpvt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))

(defgeneric rseq (obj)
  (:method ((vector bpvt)) (let ((count (count vector)))
                             (when (pos? count)
                               (let ((index (dec count)))
                                 (labels ((next* (i)
                                            (when (>= i 0)
                                              (let ((value (get vector i)))
                                                (lseq value (next* (1- i)))))))
                                   (lseq (get vector index) (next* (1- index)))))))))
