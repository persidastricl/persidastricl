;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   node.lisp
;;;
;;; classes for nodes
;;;
;;; -----

(in-package #:node)

(defclass node ()
  ((dmap :initarg :dmap :reader :dmap :documentation "bitmap-vector for value data")
   (nmap :initarg :nmap :reader :nmap :documentation "bitmap-vector for node data"))
  (:default-initargs :dmap bv:EMPTY :nmap bv:EMPTY))

(defparameter EMPTY (make-instance 'node))

(defun insert-item (node entry bits)
  (make-instance 'node :dmap (bv:insert (:dmap node) bits entry) :nmap (:nmap node)))

(defun update-item (node entry bits)
  (make-instance 'node :dmap (bv:update (:dmap node) bits entry) :nmap (:nmap node)))

(defun delete-item (node bits)
  (make-instance 'node :dmap (bv:delete (:dmap node) bits) :nmap (:nmap node)))

(defun insert-node (node new-node bits)
  (make-instance 'node :dmap (:dmap node) :nmap (bv:insert (:nmap node) bits new-node)))

(defun update-node (node new-node bits)
  (make-instance 'node :dmap (:dmap node) :nmap (bv:update (:nmap node) bits new-node)))

(defun delete-node (node bits)
  (make-instance 'node :dmap (:dmap node) :nmap (bv:delete (:nmap node) bits)))

(defun put (node entry context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth) context
      (let ((key (elt entry 0))
            (bits (b:bits hash depth)))
        ;; do we have data already?
        (if (bv:set? dmap bits)
            (let* ((current-entry (bv:get dmap bits))
                   (current-key (e:key current-entry)))
              ;; do we have the same key?
              (if (equal key current-key)
                  (update-item node entry bits)
                  (let ((new-node (-> EMPTY
                                    (put current-entry (list (h:hash current-key) (1+ depth)))
                                    (put entry         (list hash (1+ depth))))))
                    (-> node
                      (delete-item bits)
                      (insert-node new-node bits)))))
            ;; no data, so do we have a node already then for this depth
            (if (bv:set? nmap bits)
                (update-node node (put (bv:get nmap bits) entry (list hash (1+ depth))) bits)
                ;; no data, no node, so just add the entry to this node
                (insert-item node entry bits)))))))

(defun get (node key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth default) context
      (let ((bits (b:bits hash depth)))
        (if (bv:set? dmap bits)
            (let ((target (bv:get dmap bits)))
              (if (equal key (e:key target))
                  (e:value target)
                  default))
            (if (bv:set? nmap bits)
                (get (bv:get nmap bits) key (list hash (1+ depth) default))
                default))))))

(defun single-value-node? (node)
  (and (= (c:count (:dmap node)) 1)
       (= (c:count (:nmap node)) 0)))

(defun delete (node key context)
  (with-slots (dmap nmap) node
    (destructuring-bind (hash depth default) context
      (let ((bits (b:bits hash depth)))
        (if (bv:set? dmap bits)
            (delete-item node bits)
            (if (bv:set? nmap bits)
                (let* ((new-node (delete (bv:get nmap bits) key (list hash (1+ depth) default))))
                  (if (single-value-node? new-node)
                      (let ((keep (elt (:data (:dmap new-node)) 0)))
                        (-> node
                          (delete-node bits)
                          (insert-item keep bits)))
                      (update-node node new-node bits)))
                default))))))
