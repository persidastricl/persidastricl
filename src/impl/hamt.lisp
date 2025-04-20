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
;;;   hamt.lisp
;;;
;;; base class for persistent/transient hashed array mapped trie classes (sets, maps, etc)
;;;
;;; -----

(in-package #:persidastricl)

(defvar *print-hamt-items* 100)

(defclass hamt (metadata seqable) ())

(defmethod cl-murmurhash:murmurhash ((object hamt) &key (seed cl-murmurhash:*default-seed*) mix-only)
  (cl-murmurhash:murmurhash (->list object) :seed seed :mix-only mix-only))

(defmethod count ((object hamt))
  (count (seq object)))

(defmethod first ((object hamt))
  (first (seq object)))

(defmethod rest ((object hamt))
  (rest (seq object)))

(defmethod last ((object hamt))
  (last (seq object)))

(defmethod butlast ((object hamt) &optional (n 1))
  (into {} (butlast (seq object) n)))

(defmethod next ((object hamt))
  (next (seq object)))

(defmethod head ((object hamt))
  (head (seq object)))

(defmethod tail ((object hamt))
  (tail (seq object)))
