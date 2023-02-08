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

(defmethod put-it ((node hash-map-node) entry context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size) context
      (let ((key (e:key entry))
            (value (e:value entry))
            (position (b:bits hash depth bit-size)))

        (cond
          ;; do we have a node for this hash slice at this depth
          ((is-set nmap position)
           (update node position (put-it (at-position nmap position) entry (list hash (1+ depth) bit-size))))

          ;; do we have data for this hash at this depth
          ((is-set dmap position)
           (let* ((current (at-position dmap position))
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
                                   (put-it current (list (h:hash current-key) (1+ depth) bit-size))
                                   (put-it entry    (list hash (1+ depth) bit-size)))))
                   (-> node
                     (remove position)
                     (insert position new-node))))))

          ;; otherwise no node, no data, so just add the entry to this node
          (t (insert node position entry)))))))

(defmethod get-it ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size default) context
      (let ((position (b:bits hash depth bit-size)))

        (cond
          ;; do we have a node for this hash at this depth
          ((is-set nmap position)
           (get-it (at-position nmap position) key (list hash (1+ depth) bit-size default)))

          ;; do we have data for this hash at this depth
          ((is-set dmap position)
           (let ((target (at-position dmap position)))
             (if (== key (e:key target))
                 (e:value target)
                 default)))

          ;; we got nothing
          (t default))))))


(defmethod del-it ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size) context
      (let ((position (b:bits hash depth bit-size)))

        (cond
          ;; do we have a node for this hash at this depth
          ((is-set nmap position)
           (let* ((sub-node (at-position nmap position))
                  (new-node (del-it sub-node key (list hash (1+ depth) bit-size))))
             (if (single-value-node? new-node)
                 (let ((keep (single-remaining-data new-node)))
                   (-> node
                     (remove position)
                     (insert position keep)))
                 (update node position new-node))))

          ;; do we have data for this hash at this level
          ((is-set dmap position)
           (let* ((current (at-position dmap position))
                  (current-key (e:key current)))
             (when (== key current-key)
               (remove node position)))))))))
