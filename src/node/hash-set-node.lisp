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
;;;   hash-set-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  hash-set-node
;;
;;  mixin class for both persistent/transient *-hash-set-nodes

(defclass hash-set-node (hamt-node) ())

(defmethod add ((node hash-set-node) item &key hash depth)
  (with-slots (dmap nmap) node
    (let ((position (b:bits hash depth)))
      (cond
        ;; do we have a node for this hash at this depth
        ((is-set nmap position)
         (let* ((sub-node (at-position nmap position))
                (new-node (add (at-position nmap position) item :hash hash :depth (1+ depth))))
           (if (eq new-node sub-node)
               node
               (upd node position new-node))))

        ;; do we have data for this hash at this depth
        ((is-set dmap position)
         (let* ((current (at-position dmap position)))
           ;; do we have the same item?
           (if (== item current)
               node
               (let ((new-node (-> (empty-node node :hash hash :depth depth)
                                 (add current :hash (h:hash current) :depth (1+ depth))
                                 (add item    :hash hash             :depth (1+ depth)))))
                 (-> node
                   (del position)
                   (ins position new-node))))))

        ;; no data, no node, so just add the item to this node
        (t (ins node position item))))))

(defmethod loc ((node hash-set-node) item &key hash depth (default nil))
  (with-slots (dmap nmap) node
    (let ((position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash at this depth
        ((is-set nmap position)
         (loc (at-position nmap position) item :hash hash :depth (1+ depth) :default default))

        ;; do we have data for this hash at this depth
        ((is-set dmap position)
         (let ((target (at-position dmap position)))
           (if (== item target)
               target
               default)))

        ;; we got nothing
        (t default)))))

(defmethod remove ((node hash-set-node) item &key hash depth)
  (with-slots (dmap nmap) node
    (let ((position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash at this depth
        ((is-set nmap position)
         (let* ((sub-node (at-position nmap position))
                (new-node (remove sub-node item :hash hash :depth (1+ depth))))
           (if (single-value-node? new-node)
               (let ((keep (single-remaining-data new-node)))
                 (-> node
                   (del position)
                   (ins position keep)))
               (upd node position new-node))))

        ;; do we have data for this hash at this depth
        ((is-set dmap position)
         (let* ((current (at-position dmap position)))
           (if (== item current)
               (del node position)
               node)))

        ;; we have nothing so return original node
        (t node)))))
