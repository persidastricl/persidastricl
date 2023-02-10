;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-map-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  hash-map-node
;;
;; mixin class for both persistent/transient -hash-map-nodes

(defclass hash-map-node (hamt-node) ())

(defmethod put ((node hash-map-node) entry context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((key (e:key entry))
            (value (e:value entry))
            (position (b:bits hash depth)))

        (cond
          ;; do we have a node for this hash slice at this depth
          ((bv:is-set nmap position)
           (let* ((sub-node (bv:at-position nmap position))
                  (new-node (put sub-node entry (list hash (1+ depth)))))
             (if (eq new-node sub-node)
                 node
                 (update node position new-node))))

          ;; do we have data for this hash at this depth
          ((bv:is-set dmap position)
           (let* ((current (bv:at-position dmap position))
                  (current-key (e:key current))
                  (current-value (e:value current)))

             ;; do we have the same key?
             (if (== key current-key)
                 ;; do we have the same value
                 (if (== value current-value)
                     node
                     (update node position entry))

                 ;; different key with same hash at this depth
                 (let ((new-node (-> (empty-node node context)
                                   (put current (list (h:hash current-key) (1+ depth)))
                                   (put entry    (list hash (1+ depth))))))
                   (-> node
                     (remove position)
                     (insert position new-node))))))

          ;; otherwise no node, no data, so just add the entry to this node
          (t (insert node position entry)))))))

(defmethod get ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth default) context
      (let ((position (b:bits hash depth)))

        (cond
          ;; do we have a node for this hash at this depth
          ((bv:is-set nmap position)
           (get (bv:at-position nmap position) key (list hash (1+ depth) default)))

          ;; do we have data for this hash at this depth
          ((bv:is-set dmap position)
           (let ((target (bv:at-position dmap position)))
             (if (== key (e:key target))
                 (e:value target)
                 default)))

          ;; it is not here at all so return default
          (t default))))))


(defmethod delete ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((position (b:bits hash depth)))

        (cond
          ;; do we have a node for this hash at this depth
          ((bv:is-set nmap position)
           (let* ((sub-node (bv:at-position nmap position))
                  (new-node (delete sub-node key (list hash (1+ depth)))))
             (if (eq new-node sub-node)
                 node
                 (if (single-value-node? new-node)
                     (let ((keep (single-remaining-data new-node)))
                       (-> node
                         (remove position)
                         (insert position keep)))
                     (update node position new-node)))))

          ;; do we have data for this hash at this level
          ((bv:is-set dmap position)
           (let* ((current (bv:at-position dmap position))
                  (current-key (e:key current)))
             (when (== key current-key)
               (remove node position))))

          ;; we have nothing so return original node
          (t node))))))
