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
;;;   hash-map-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  hash-map-node
;;
;; mixin class for both persistent/transient -hash-map-nodes

(defclass hash-map-node (hamt-node) ())

(defmethod nth-value ((node hash-map-node) index)
  (with-slots (dvec) node
    (let ((idx (* 2 index)))
      (map-entry (elt dvec idx) (elt dvec (1+ idx))))))

(defmethod value-at ((node hash-map-node) position)
  (with-slots (dmap) node
    (when (b:set? position dmap)
      (nth-value node (b:index position dmap)))))

(defmethod add ((node hash-map-node) entry &key hash depth)
  (with-slots (dmap nmap) node
    (let ((key (key entry))
          (value (value entry))
          (position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash slice at this depth
        ((b:set? position nmap)
         (let* ((sub-node (subnode-at node position))
                (new-node (add sub-node entry :hash hash :depth (1+ depth))))
           (if (eq new-node sub-node)
               node
               (upd node position new-node))))

        ;; do we have data for this hash at this depth
        ((b:set? position dmap)
         (let* ((current (value-at node position))
                (current-key (key current))
                (current-value (value current)))

           ;; do we have the same key?
           (if (== key current-key)
               ;; do we have the same value
               (if (== value current-value)
                   node
                   (upd node position entry))

               ;; different key with same hash at this depth
               (let ((new-node (-> (empty-node node :hash hash :depth depth)
                                 (add current :hash (h:hash current-key) :depth (1+ depth))
                                 (add entry   :hash hash                 :depth (1+ depth)))))
                 (-> node
                   (del position)
                   (ins position new-node))))))

        ;; otherwise no node, no data, so just add the entry to this node
        (t (ins node position entry))))))

(defmethod loc ((node hash-map-node) key &key hash depth (default nil))
  (with-slots (dmap nmap) node
    (let ((position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash at this depth
        ((b:set? position nmap)
         (loc (subnode-at node position) key :hash hash :depth (1+ depth) :default default))

        ;; do we have data for this hash at this depth
        ((b:set? position dmap)
         (let ((target (value-at node position)))
           (if (== key (key target))
               (value target)
               default)))

        ;; it is not here at all so return default
        (t default)))))

(defmethod remove ((node hash-map-node) key &key hash depth)
  (with-slots (dmap nmap) node
    (let ((position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash at this depth
        ((b:set? position nmap)
         (let* ((sub-node (subnode-at node position))
                (new-node (remove sub-node key :hash hash :depth (1+ depth))))
           (if (single-value-node? new-node)
               (let ((keep (single-remaining-data new-node)))
                 (-> node
                     (del position)
                     (ins position keep)))
               (upd node position new-node))))

        ;; do we have data for this hash at this level
        ((b:set? position dmap)
         (let* ((current (value-at node position))
                (current-key (key current)))
           (if (== key current-key)
               (del node position)
               node)))

        ;; we have nothing so return original node
        (t node)))))
