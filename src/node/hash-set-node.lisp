;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-set-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  hash-set-node
;;
;;  mixin class for both persistent/transient *-hash-set-nodes

(defclass hash-set-node (hamt-node) ())

(defmethod put ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((position (b:bits hash depth)))

        (cond
          ;; do we have a node for this hash at this depth
          ((bv:is-set nmap position)
           (let* ((sub-node (bv:at-position nmap position))
                  (new-node (put (bv:at-position nmap position) item (list hash (1+ depth)))))
             (if (eq new-node sub-node)
                 node
                 (update node position new-node))))

          ;; do we have data for this hash at this depth
          ((bv:is-set dmap position)
           (let* ((current (bv:at-position dmap position)))
             ;; do we have the same item?
             (if (== item current)
                 node
                 (let ((new-node (-> (empty-node node context)
                                   (put current (list (h:hash current) (1+ depth)))
                                   (put item    (list hash (1+ depth))))))
                   (-> node
                     (remove position)
                     (insert position new-node))))))

          ;; no data, no node, so just add the item to this node
          (t (insert node position item)))))))

(defmethod get ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth default) context
      (let ((position (b:bits hash depth)))

        (cond
          ;; do we have a node for this hash at this depth
          ((bv:is-set nmap position)
           (get (bv:at-position nmap position) item (list hash (1+ depth) default)))

          ;; do we have data for this hash at this depth
          ((bv:is-set dmap position)
           (let ((target (bv:at-position dmap position)))
             (if (== item target)
                 target
                 default)))

          ;; we got nothing
          (t default))))))

(defmethod delete ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((position (b:bits hash depth)))

        (cond
          ;; do we have a node for this hash at this depth
          ((bv:is-set nmap position)
           (let* ((sub-node (bv:at-position nmap position))
                  (new-node (delete sub-node item (list hash (1+ depth)))))
             (if (eq new-node sub-node)
                 node
                 (if (single-value-node? new-node)
                     (let ((keep (single-remaining-data new-node)))
                       (-> node
                         (remove position)
                         (insert position keep)))
                     (update node position new-node)))))

          ;; do we have data for this hash at this depth
          ((bv:is-set dmap position)
           (let* ((current (bv:at-position dmap position)))
             (when (== item current)
               (remove node position))))

          ;; we have nothing so return original node
          (t node))))))
