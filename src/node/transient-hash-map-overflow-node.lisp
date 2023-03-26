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
;;;   transient-hash-map-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  transient-hash-map-overflow-node
;;
;; -----

(defclass transient-hash-map-overflow-node (transient-overflow-node hash-map-overflow-node) ())

(defmethod add ((node transient-hash-map-overflow-node) entry &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (let ((key (key entry))
        (value (value entry)))
    (unless hash (setf (hash node) hash))
    (setf (data node) (->> (data node)
                        (remove-if (lambda (e) (== (car e) key)))
                        (acons key value))))
  node)

(defmethod remove ((node transient-hash-map-overflow-node) key &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (unless (hash node) (setf (hash node) hash))
  (setf (data node) (remove-if (lambda (e) (== (car e) key)) (data data)))
  node)
