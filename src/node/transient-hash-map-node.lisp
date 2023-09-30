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
;;;   transient-hash-map.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-map-node
;;
;; -----

(defclass transient-hash-map-node (hash-map-node) ())

(defmethod ins ((node transient-hash-map-node) position (new-node transient-hash-map-node))
  (with-slots (nmap nvec) node
    (setf nmap (b:set position nmap))
    (setf nvec (v:insert nvec (b:index position nmap) new-node)))
  node)

(defmethod ins ((node transient-hash-map-node) position (new-node transient-hash-map-overflow-node))
  (with-slots (nmap nvec) node
    (setf nmap (b:set position nmap))
    (setf nvec (v:insert nvec (b:index position nmap) new-node)))
  node)

(defmethod ins ((node transient-hash-map-node) position (entry entry))
  (with-slots (dmap dvec) node
    (setf dmap (b:set position dmap))
    (setf dvec (v:insert dvec (* (b:index position dmap) 2) (key entry) (value entry))))
  node)

(defmethod upd ((node transient-hash-map-node) position (new-node transient-hash-map-node))
  (with-slots (nmap nvec) node
    (setf nvec (v:modify nvec (b:index position nmap) new-node)))
  node)

(defmethod upd ((node transient-hash-map-node) position (new-node transient-hash-map-overflow-node))
  (with-slots (nmap nvec) node
    (setf nvec (v:modify nvec (b:index position nmap) new-node)))
  node)

(defmethod upd ((node transient-hash-map-node) position (entry entry))
  (with-slots (dmap dvec) node
    (v:modify dvec (1+ (* (b:index position dmap) 2)) (value entry)))
  node)

(defmethod del ((node transient-hash-map-node) position)
  (with-slots (dmap dvec nmap nvec) node
    (cond
      ((b:set? position dmap)
       (setf dmap (b:clear position dmap))
       (setf dvec (v:delete dvec (* (b:index position dmap) 2) 2)))

      ((b:set? position nmap)
       (setf nmap (b:clear position nmap))
       (setf nvec (v:delete nvec (b:index position nmap))))

      (t (error "critical error: attempt to del position in transient-hash-map-node that is not set for either data or subnode!"))))
  node)
