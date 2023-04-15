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
;;;   transient-hash-set-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; transient-hash-set-node
;;
;; -----

(defclass transient-hash-set-node (hash-set-node) ())

(defmethod ins ((node transient-hash-set-node) position (new-node transient-hash-set-node))
  (with-slots (nmap nvec) node
    (setf nmap (b:set position nmap))
    (setf nvec (v:insert nvec (b:index position nmap) new-node)))
  node)

(defmethod ins ((node transient-hash-set-node) position (new-node transient-hash-set-overflow-node))
  (with-slots (nmap nvec) node
    (setf nmap (b:set position nmap))
    (setf nvec (v:insert nvec (b:index position nmap) new-node)))
  node)

(defmethod ins ((node transient-hash-set-node) position item)
  (with-slots (dmap dvec) node
    (setf dmap (b:set position dmap))
    (setf dvec (v:insert dvec (b:index position dmap) item)))
  node)

(defmethod upd ((node transient-hash-set-node) position (new-node transient-hash-set-node))
  (with-slots (nmap nvec) node
    (setf nvec (v:modify nvec (b:index position nmap) new-node)))
  node)

(defmethod upd ((node transient-hash-set-node) position (new-node transient-hash-set-overflow-node))
  (with-slots (nmap nvec) node
    (setf nvec (v:modify nvec (b:index position nmap) new-node)))
  node)

(defmethod upd ((node transient-hash-set-node) position item)
  (with-slots (dmap dvec) node
    (setf dvec (v:modify dvec (b:index position dmap) item)))
  node)

(defmethod del ((node transient-hash-set-node) position)
  (with-slots (dmap dvec nmap nvec) node
    (cond
      ((b:set? position dmap)
       (setf dmap (b:clear position dmap))
       (setf dvec (v:delete dvec (b:index position dmap))))

      ((b:set? posistion nmap)
       (setf nmap (b:clear position nmap))
       (setf nvec (v:delete nvec (b:index position nmap))))

      (t (error "critical error: attempt to del position in transient-hash-set-node that is not set for either data or subnode!"))))
  node)
