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
;;;   hash-map-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  hash-map-overflow-node
;;
;; -----

(defclass hash-map-overflow-node (overflow-node) ())

(defmethod nth-value ((node hash-map-overflow-node) index)
  (let ((e (elt (data node) index)))
    (map-entry (first e) (rest e))))

(defmethod single-remaining-data ((node hash-map-overflow-node))
  (let ((target (first (data node))))
    (map-entry (first target) (rest target))))

(defmethod loc ((node hash-map-overflow-node) key &key hash (default nil) &allow-other-keys)
  (when (hash node) (assert (= (hash node) hash)))
  (if-let ((target (cl:assoc key (data node) :test #'==)))
    (rest target)
    default))
