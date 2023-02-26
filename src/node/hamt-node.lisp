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
;;  a bitmap/data bitmap-vector for data items (dmap)
;;  a bitmap/data bitmap vector for subnodes (nmap)
;;
;; in the paper below explaining optimized HAMT structures, the vector
;; storing the data and the subnodes is a single vecor and indexed
;; from both ends using two separate bitmaps.  Alternatively, the
;; nodes here in this implementation uses two distinct vectors (one
;; for each bitmap and indexed normally,) elliminating the need to do
;; any special vector offset arithmetic to store the subnode data from
;; the opposite end of the single vector. No evidence of improved or
;; even equal efficiency is offered. It only seemed to make sense and
;; made things a bit easier (no pun intended).
;;
;; (see 'https://michael.steindorfer.name/publications/oopsla15.pdf')
;; -----

(defclass hamt-node (node)
  ((dmap :initarg :dmap :reader dmap :documentation "bitmap-vector for value data")
   (nmap :initarg :nmap :reader nmap :documentation "bitmap-vector for node data")))

(defgeneric sub-nodes (node)
  (:method ((node hamt-node)) (data (nmap node))))

(defgeneric single-value-node? (node)
  (:method  ((node hamt-node)) (and (= (count (dmap node)) 1)
                                    (= (count (nmap node)) 0))))

(defgeneric single-remaining-data (node)
  (:method ((node hamt-node)) (at-index (dmap node) 0)))

(defmethod at-index ((node hamt-node) index)
  (at-index (dmap node) index))

(defmethod count ((node hamt-node))
  (count (dmap node)))

(defmethod empty? ((node hamt-node))
  (and (zerop (count (dmap node)))
       (zerop (count (nmap node)))))
