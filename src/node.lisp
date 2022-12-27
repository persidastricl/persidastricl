;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric put (target item context))
(defgeneric del (target item context))
(defgeneric get (target item context))

(defclass node ()
  ((dmap :initarg :dmap :reader :dmap :documentation "bitmap-vector for value data")
   (nmap :initarg :nmap :reader :nmap :documentation "bitmap-vector for node data")))

(defun single-value-node? (node)
  (and (= (count (:dmap node)) 1)
       (= (count (:nmap node)) 0)))

(defclass transient-node (node) ())
(define-immutable-class persistent-node (node) ())

;; -----
;;  hash-set-node
;;
;;  mixin class for both persistent/transient -hash-set-nodes

(defclass hash-set-node (node) ())

(defmethod put ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((position (b:bits hash depth)))
        ;; do we have data already?
        (if (is-set dmap position)
            (let* ((current (at-position dmap position)))
              ;; do we have the same item?
              (if (== item current)
                  node
                  (let ((new-node (-> (empty node)
                                    (put current (list (h:hash current) (1+ depth)))
                                    (put item    (list hash (1+ depth))))))
                    (-> node
                      (remove position)
                      (insert position new-node)))))
            ;; no data, so do we have a node already then for this depth
            (if (is-set nmap position)
                (update node position (put (at-position nmap position) item (list hash (1+ depth))))
                ;; no data, no node, so just add the item to this node
                (insert node position item)))))))

(defmethod get ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth default) context
      (let ((position (b:bits hash depth)))
        (if (is-set dmap position)
            (let ((target (at-position dmap position)))
              (if (== item target)
                  target
                  default))
            (if (is-set nmap position)
                (get (at-position nmap position) item (list hash (1+ depth) default))
                default))))))

(defmethod del ((node hash-set-node) item context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((position (b:bits hash depth)))
        (if (is-set dmap position)
            (let* ((current (at-position dmap position)))
              (when (== item current)
                (remove node position)))
            (when (is-set nmap position)
              (let* ((sub-node (at-position nmap position))
                     (new-node (del sub-node item (list hash (1+ depth)))))
                (if (single-value-node? new-node)
                    (let ((keep (at-index (:dmap new-node) 0)))
                      (-> node
                        (remove position)
                        (insert position keep)))
                    (update node position new-node)))))))))

(defmethod contains? ((node hash-set-node) item)
  (not (== :not-found (get node item (list (h:hash item) 0 :not-found)))))


;; -----
;;  hash-map-node
;;
;; mixin class for both persistent/transient -hash-map-nodes

(defclass hash-map-node (node) ())

(defmethod put ((node hash-map-node) entry context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((key (e:key entry))
            (position (b:bits hash depth)))
        ;; do we have data already?
        (if (is-set dmap position)
            (let* ((current (at-position dmap position))
                   (current-key (e:key current)))
              ;; do we have the same key?
              (if (equal key current-key)
                  (update node position entry)
                  (let ((new-node (-> (empty node)
                                    (put current (list (h:hash current-key) (1+ depth)))
                                    (put entry    (list hash (1+ depth))))))
                    (-> node
                      (remove position)
                      (insert position new-node)))))
            ;; no data, so do we have a node already then for this depth
            (if (is-set nmap position)
                (update node position (put (at-position nmap position) entry (list hash (1+ depth))))
                ;; no data, no node, so just add the entry to this node
                (insert node position entry)))))))

(defmethod get ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth default) context
      (let ((position (b:bits hash depth)))
        (if (is-set dmap position)
            (let ((target (at-position dmap position)))
              (if (equal key (e:key target))
                  (e:value target)
                  default))
            (if (is-set nmap position)
                (get (at-position nmap position) key (list hash (1+ depth) default))
                default))))))

(defmethod del ((node hash-map-node) key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((position (b:bits hash depth)))
        (if (is-set dmap position)
            (let* ((current (at-position dmap position))
                   (current-key (e:key current)))
              (when (equal key current-key)
                (remove node position)))
            (when (is-set nmap position)
              (let* ((sub-node (at-position nmap position))
                     (new-node (del sub-node key (list hash (1+ depth)))))
                (if (single-value-node? new-node)
                    (let ((keep (at-index (:dmap new-node) 0)))
                      (-> node
                        (remove position)
                        (insert position keep)))
                    (update node position new-node)))))))))
