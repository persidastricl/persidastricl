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

;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

(define-immutable-class persistent-vector (vector) ()
  (:default-initargs :root (make-instance 'persistent-vector-node) :tail-end (make-instance 'persistent-vector-leaf-node) :tail-offset 0 :meta nil))

(defmethod cons (value (pv persistent-vector))
  (labels ((new-root? (node)
             (with-slots (data level) node
               (and (= (count data) 32)
                    (if (> level 1)
                        (new-root? (elt data 31))
                        t)))))
    (with-slots (root count tail-offset tail-end meta) pv
      (let ((index count)
            (new-tail? (= (count tail-end) 32)))

        (if new-tail?
            (let* ((new-tail (cons value (make-instance 'persistent-vector-leaf-node)))
                   (new-root (if (new-root? root)
                                 (let ((nr (make-instance 'persistent-vector-node :data (make-array 1 :initial-element root)
                                                                                  :level (1+ (level root)))))
                                   (add-leaf-node nr tail-end tail-offset))
                                 (add-leaf-node root tail-end tail-offset))))
              (make-instance 'persistent-vector :root new-root :tail-end new-tail :tail-offset index :count (1+ count) :meta meta))

            (make-instance 'persistent-vector :root root :tail-end (cons value tail-end) :tail-offset tail-offset :count (1+ count) :meta meta))))))

(defmethod pop ((pv persistent-vector))
  (with-slots (root count tail-offset tail-end meta) pv
    (let ((new-tail (pop tail-end)))
      (if (and (> tail-offset 0) (= (count new-tail) 0))
          (let* ((new-tail-offset (max 0 (- tail-offset 32)))
                 (leaf-node (get-leaf-node root new-tail-offset))
                 (new-root (remove-leaf-node root new-tail-offset)))
            (make-instance 'persistent-vector
                           :root (if (= (count new-root) 1)
                                     (elt (slot-value new-root 'data) 0)
                                     new-root)
                           :tail-end leaf-node
                           :tail-offset new-tail-offset
                           :count (1- count)))
          (make-instance 'persistent-vector :root root :tail-end new-tail :tail-offset tail-offset :count (1- count) :meta meta)))))

(defgeneric vec (object)
  (:method (obj) (into (persistent-vector) (seq obj))))

(defgeneric ->vec (object)
  (:method (obj) (into (persistent-vector) (seq obj))))

(defun persistent-vector (&rest items)
  (cl:reduce
   (lambda (pv item)
     (cons item pv))
   items
   :initial-value (make-instance 'persistent-vector)))

(defun pprint-persistent-vector (stream pv &rest other-args)
  (declare (ignore other-args))
  (let ((*print-length* (min *print-bpvt-items* (or *print-lines* *print-bpvt-items*))))
    (pprint-logical-block (stream (->list (take (inc *print-length*) (seq pv))) :prefix "[" :suffix "]")
      (pprint-exit-if-list-exhausted)
      (loop
        (write (pprint-pop) :stream stream)
        (pprint-exit-if-list-exhausted)
        (write-char #\space stream)
        (pprint-newline :fill stream)))))

(defmethod print-object ((object persistent-vector) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-persistent-vector/" object)
      (format stream "(persidastricl:persistent-vector ~{~s~^ ~})" (->list object))))

(set-pprint-dispatch 'persistent-vector 'pprint-persistent-vector)

(defmethod make-load-form ((object persistent-vector) &optional env)
  (declare (ignore env))
  (let ((items (->list object)))
    `(persidastricl::persistent-vector ,@items)))

(defmethod assoc ((pv persistent-vector) index item &rest kv-pairs)
  (cl:reduce
   (lambda (pv kv-pair)
     (with-slots (root tail-end count tail-offset meta) pv
       (destructuring-bind (idx item) kv-pair
         (cond
           ((= idx (count pv))
            (cons item pv))

           ((>= idx tail-offset)
            (make-instance 'persistent-vector :root root :count count :tail-offset tail-offset :tail-end (add tail-end item :index idx) :meta meta))

           (t
            (make-instance 'persistent-vector :root (add root item :index idx) :count count :tail-offset tail-offset :tail-end tail-end) :meta meta)))))
   (->list (partition-all (list* index item kv-pairs) 2))
   :initial-value pv))

(defmethod with-meta ((pv persistent-vector) meta)
  (with-slots (root count tail-end tail-offset) pv
    (make-instance 'persistent-vector :root root :tail-end tail-end :tail-offset tail-offset :count count :meta meta)))

(defmethod update-instance-for-different-class :before ((old transient-vector)
                                                        (new persistent-vector)
                                                        &key)
  (slot-makunbound new 'root)
  (slot-makunbound new 'tail-end)
  (slot-makunbound new 'tail-offset)
  (slot-makunbound new 'count)

  (with-slots (root tail-end tail-offset count) old
    (setf (slot-value new 'root) (change-class root 'persistent-vector-node))
    (setf (slot-value new 'tail-end) (change-class tail-end 'persistent-vector-leaf-node))
    (setf (slot-value new 'tail-offset) tail-offset)
    (setf (slot-value new 'count) count)))
