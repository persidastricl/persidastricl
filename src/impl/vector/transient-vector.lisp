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
;;; transient-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass transient-vector (vector) ()
  (:default-initargs :root (make-instance 'transient-vector-node) :tail-end (make-instance 'transient-vector-leaf-node) :tail-offset 0))

(defmethod cons (value (tv transient-vector))
  (labels ((new-root? (node)
             (with-slots (data level) node
               (and (= (count data) 32)
                    (if (> level 1)
                        (new-root? (elt data 31))
                        t)))))
    (with-slots (root count tail-offset tail-end) tv
      (let ((index count)
            (new-tail? (= (count tail-end) 32)))

        (if new-tail?
            (let* ((new-tail (cons value (make-instance 'transient-vector-leaf-node)))
                   (new-root (if (new-root? root)
                                 (let ((nr (make-instance 'transient-vector-node :level (1+ (level root)))))
                                   (with-slots (data) nr
                                     (vector-push root data))
                                   (add-leaf-node nr tail-end tail-offset))
                                 (add-leaf-node root tail-end tail-offset))))
              (setf root new-root
                    tail-end new-tail
                    tail-offset index))
            ;; no need for a new tail-end
            (cons value tail-end))

        (setf count (1+ count)))))
  tv)

(defmethod pop ((tv transient-vector))
  (with-slots (root count tail-offset tail-end) tv
    (let ((new-tail (pop tail-end)))
      (if (and (> tail-offset 0) (= (count new-tail) 0))
          (let* ((new-tail-offset (max 0 (- tail-offset 32)))
                 (leaf-node (get-leaf-node root new-tail-offset))
                 (new-root (remove-leaf-node root new-tail-offset)))
            (setf root (if (= (count new-root) 1)
                           (elt (slot-value new-root 'data) 0)
                           new-root)
                  tail-end leaf-node
                  tail-offset new-tail-offset))
          (setf tail-end new-tail)))
    (setf count (1- count)))
  tv)

(defgeneric t-vec (object)
  (:method (obj) (into (transient-vector) (seq obj))))

(defun transient-vector (&rest items)
  (cl:reduce
   (lambda (tv item)
     (cons item tv))
   items
   :initial-value (make-instance 'transient-vector)))

(defun pprint-transient-vector (stream pv &rest other-args)
  (declare (ignore other-args))
  (pprint-logical-block (stream (->list pv) :prefix "@[" :suffix "]")
    (pprint-exit-if-list-exhausted)
    (loop
      (write (pprint-pop) :stream stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :fill stream))))

(defmethod print-object ((obj transient-vector) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "~/persidastricl::pprint-transient-vector/" obj)
      (format stream "(persidastricl:transient-vector  ~{~s~^ ~})" (->list obj))))

(set-pprint-dispatch 'transient-vector 'pprint-transient-vector)

(defmethod make-load-form ((obj transient-vector) &optional env)
  (declare (ignore env))
  (let ((items (flatten (seq obj))))
    `(persidastricl::transient-vector ,@items)))

(defmethod assoc ((tv transient-vector) index item &rest kv-pairs)
  (with-slots (root tail-end tail-offset) tv
    (cl:reduce
     (lambda (tv kv-pair)
       (destructuring-bind (idx item) kv-pair
         (if (>= idx tail-offset)
             (setf tail-end (add tail-end item :index idx))
             (setf root (add root item :index idx)))
         tv))
     (->list (partition-all (list* index item kv-pairs) 2))
     :initial-value tv)))

(defmethod with-meta ((tv transient-vector) (meta hash-map))
  (setf (slot-value tv 'meta) meta)
  tv)
