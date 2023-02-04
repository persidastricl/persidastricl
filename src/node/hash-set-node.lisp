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

(defmethod put-it ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size) context
      (let ((position (b:bits hash depth bit-size)))
        ;; do we have data already?
        (if (is-set dmap position)
            (let* ((current (at-position dmap position)))
              ;; do we have the same item?
              (if (== item current)
                  node
                  (let ((new-node (-> (empty-node node context)
                                    (put-it current (list (h:hash current) (1+ depth) bit-size))
                                    (put-it item    (list hash (1+ depth) bit-size)))))
                    (-> node
                      (remove position)
                      (insert position new-node)))))
            ;; no data, so do we have a node already then for this depth
            (if (is-set nmap position)
                (update node position (put-it (at-position nmap position) item (list hash (1+ depth) bit-size)))
                ;; no data, no node, so just add the item to this node
                (insert node position item)))))))

(defmethod get-it ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size default) context
      (let ((position (b:bits hash depth bit-size)))
        (if (is-set dmap position)
            (let ((target (at-position dmap position)))
              (if (== item target)
                  target
                  default))
            (if (is-set nmap position)
                (get-it (at-position nmap position) item (list hash (1+ depth) bit-size default))
                default))))))

(defmethod del-it ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size) context
      (let ((position (b:bits hash depth bit-size)))
        (if (is-set dmap position)
            (let* ((current (at-position dmap position)))
              (when (== item current)
                (remove node position)))
            (when (is-set nmap position)
              (let* ((sub-node (at-position nmap position))
                     (new-node (del-it sub-node item (list hash (1+ depth) bit-size))))
                (if (single-value-node? new-node)
                    (let ((keep (single-remaining-data new-node)))
                      (-> node
                        (remove position)
                        (insert position keep)))
                    (update node position new-node)))))))))
