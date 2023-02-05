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
            (position (b:bits hash depth bit-size)))
        ;; do we have data already?
        (if (is-set dmap position)
            (let* ((current (at-position dmap position))
                   (current-key (e:key current)))
              ;; do we have the same key?
              (if (== key current-key)
                  (update node position entry)
                  (let ((new-node (-> (empty-node node context)
                                    (put-it current (list (h:hash current-key) (1+ depth) bit-size))
                                    (put-it entry    (list hash (1+ depth) bit-size)))))
                    (-> node
                      (remove position)
                      (insert position new-node)))))
            ;; no data, so do we have a node already then for this depth
            (if (is-set nmap position)
                (update node position (put-it (at-position nmap position) entry (list hash (1+ depth) bit-size)))
                ;; no data, no node, so just add the entry to this node
                (insert node position entry)))))))

(defmethod get-it ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size default) context
      (let ((position (b:bits hash depth bit-size)))
        (if (is-set dmap position)
            (let ((target (at-position dmap position)))
              (if (== key (e:key target))
                  (e:value target)
                  default))
            (if (is-set nmap position)
                (get-it (at-position nmap position) key (list hash (1+ depth) bit-size default))
                default))))))

(defmethod del-it ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth bit-size) context
      (let ((position (b:bits hash depth bit-size)))
        (if (is-set dmap position)
            (let* ((current (at-position dmap position))
                   (current-key (e:key current)))
              (when (== key current-key)
                (remove node position)))
            (when (is-set nmap position)
              (let* ((sub-node (at-position nmap position))
                     (new-node (del-it sub-node key (list hash (1+ depth) bit-size))))
                (if (single-value-node? new-node)
                    (let ((keep (single-remaining-data new-node)))
                      (-> node
                        (remove position)
                        (insert position keep)))
                    (update node position new-node)))))))))
