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

(defmethod add ((node hash-map-node) entry &key hash depth)
  (with-slots (dmap nmap) node
    (let ((key (e:key entry))
          (value (e:value entry))
          (position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash slice at this depth
        ((is-set nmap position)
         (let* ((sub-node (at-position nmap position))
                (new-node (add sub-node entry :hash hash :depth (1+ depth))))
           (if (eq new-node sub-node)
               node
               (upd node position new-node))))

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
        ((is-set nmap position)
         (loc (at-position nmap position) key :hash hash :depth (1+ depth) :default default))

        ;; do we have data for this hash at this depth
        ((is-set dmap position)
         (let ((target (at-position dmap position)))
           (if (== key (e:key target))
               (e:value target)
               default)))

        ;; it is not here at all so return default
        (t default)))))


(defmethod remove ((node hash-map-node) key &key hash depth)
  (with-slots (dmap nmap) node
    (let ((position (b:bits hash depth)))

      (cond
        ;; do we have a node for this hash at this depth
        ((is-set nmap position)
         (let* ((sub-node (at-position nmap position))
                (new-node (remove sub-node key :hash hash :depth (1+ depth))))
           (if (eq new-node sub-node)
               node
               (if (single-value-node? new-node)
                   (let ((keep (single-remaining-data new-node)))
                     (-> node
                       (del position)
                       (ins position keep)))
                   (upd node position new-node)))))

        ;; do we have data for this hash at this level
        ((is-set dmap position)
         (let* ((current (at-position dmap position))
                (current-key (e:key current)))
           (when (== key current-key)
             (del node position))))

        ;; we have nothing so return original node
        (t node)))))
