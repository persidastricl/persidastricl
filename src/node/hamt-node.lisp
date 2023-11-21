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
;;;   hamt-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  base class for all hamt node objects
;;
;;  HAMT node that contains:
;;
;;  a bitmap/data vector for data items (dmap/dvec)
;;  a bitmap/data vector for subnodes (nmap/nvec)
;;
;; in the paper below explaining optimized HAMT structures, the vector
;; storing the data and the subnodes is a single vector and indexed
;; from both ends using two separate bitmaps.  Alternatively, the
;; nodes here in this implementation uses two distinct vectors (one
;; for each bitmap and indexed normally from the left) elliminating
;; the need to do any special vector offset arithmetic to store the
;; subnode data from the opposite end of the single vector. No
;; evidence of improved or even equal efficiency is offered. It only
;; seemed to make sense and made things a bit easier (no pun
;; intended). Alternative implementations and benchmarking are
;; warranted.
;;
;; (see 'https://michael.steindorfer.name/publications/oopsla15.pdf')
;; -----

(defclass hamt-node (node)
  ((dmap :initarg :dmap :reader dmap)
   (dvec :initarg :dvec :reader dvec)
   (nmap :initarg :nmap :reader nmap)
   (nvec :initarg :nvec :reader nvec))
  (:default-initargs :dmap 0 :dvec (make-array 0) :nmap 0 :nvec (make-array 0)))

(defgeneric sub-nodes (node)
  (:method ((node hamt-node)) (slot-value node 'nvec)))

(defgeneric single-value-node? (node)
  (:method  ((node hamt-node)) (and (= (logcount (slot-value node 'dmap)) 1)
                                    (= (logcount (slot-value node 'nmap)) 0))))

(defmethod nth-value ((node hamt-node) index)
  (elt (slot-value node 'dvec) index))

(defun nth-subnode (node index)
  (elt (slot-value node 'nvec) index))

(defmethod value-at ((node hamt-node) position)
  (with-slots (dmap) node
    (when (b:set? position dmap)
      (nth-value node (b:index position dmap)))))

(defun subnode-at (node position)
  (with-slots (nmap) node
    (when (b:set? position nmap)
      (nth-subnode node (b:index position nmap)))))

(defgeneric single-remaining-data (node)
  (:method ((node hamt-node)) (nth-value node 0)))

(defmethod count ((node hamt-node))
  (logcount (slot-value node 'dmap)))

(defmethod empty? ((node hamt-node))
  (and (zerop (logcount (slot-value node 'dmap)))
       (zerop (logcount (slot-value node 'nmap)))))
