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
;;;   persistent-hash-map-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; persistent-hash-map-node
;;
;; -----

(define-immutable-class persistent-hash-map-node (hash-map-node) ())

(defmethod ins ((node persistent-hash-map-node) position (new-node persistent-hash-map-node))
  (with-slots (dmap dvec nmap nvec) node
    (make-instance (type-of node) :dmap dmap
                                  :dvec dvec
                                  :nmap (b:set position nmap)
                                  :nvec (v:insert nvec (b:index position nmap) new-node))))

(defmethod ins ((node persistent-hash-map-node) position (new-node persistent-hash-map-overflow-node))
  (with-slots (dmap dvec nmap nvec) node
    (make-instance (type-of node) :dmap dmap
                                  :dvec dvec
                                  :nmap (b:set position nmap)
                                  :nvec (v:insert nvec (b:index position nmap) new-node))))

(defmethod ins ((node persistent-hash-map-node) position (entry entry))
  (with-slots (dmap dvec nmap nvec) node
    (make-instance (type-of node) :dmap (b:set position dmap)
                                  :dvec (v:insert dvec (* (b:index position dmap) 2) (key entry) (value entry))
                                  :nmap nmap
                                  :nvec nvec)))

(defmethod upd ((node persistent-hash-map-node) position (new-node persistent-hash-map-node))
  (with-slots (dmap dvec nmap nvec) node
    (make-instance (type-of node) :dmap dmap
                                  :dvec dvec
                                  :nmap nmap
                                  :nvec (v:update nvec (b:index position nmap) new-node))))

(defmethod upd ((node persistent-hash-map-node) position (new-node persistent-hash-map-overflow-node))
  (with-slots (dmap dvec nmap nvec) node
    (make-instance (type-of node) :dmap dmap
                                  :dvec dvec
                                  :nmap nmap
                                  :nvec (v:update nvec (b:index position nmap) new-node))))

(defmethod upd ((node persistent-hash-map-node) position (entry entry))
  (with-slots (dmap dvec nmap nvec) node
    (make-instance (type-of node) :dmap dmap
                                  :dvec (v:update dvec (1+ (* (b:index position dmap) 2)) (value entry))
                                  :nmap nmap
                                  :nvec nvec)))

(defmethod del ((node persistent-hash-map-node) position)
  (with-slots (dmap dvec nmap nvec) node
    (cond

      ((b:set? position dmap)
       (make-instance (type-of node) :dmap (b:clear position dmap)
                                     :dvec (v:delete dvec (* (b:index position dmap) 2) 2)
                                     :nmap nmap
                                     :nvec nvec))

      ((b:set? position nmap)
       (make-instance (type-of node) :dmap dmap
                                     :dvec dvec
                                     :nmap (b:clear position nmap)
                                     :nvec (v:delete nvec (b:index position nmap))))

      (t (error "critical error: attempt to del position in persisetnt-hash-map-node that is not set for either data or subnode!")))))
