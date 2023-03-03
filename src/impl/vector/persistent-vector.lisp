;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector.lisp
;;;
;;; -----

(in-package #:persidastricl)

(define-immutable-class persistent-vector (vector) ()
  (:default-initargs :root (make-instance 'persistent-vector-node) :tail (make-instance 'persistent-vector-leaf-node) :tail-offset 0))

(defmethod cons (value (pv persistent-vector))
  (labels ((new-root? (node)
             (with-slots (data level) node
               (and (= (count data) 32)
                    (if (> level 1)
                        (new-root? (elt data 31))
                        t)))))
    (with-slots (root count tail-offset tail) pv
      (let ((index count)
            (new-tail? (= (count tail) 32)))

        (if new-tail?
            (let* ((new-tail (cons value (make-instance 'persistent-vector-leaf-node)))
                   (new-root (if (new-root? root)
                                 (let ((nr (make-instance 'persistent-vector-node :data (make-array 1 :initial-element root)
                                                                                  :level (1+ (level root)))))
                                   (add-leaf-node nr tail tail-offset))
                                 (add-leaf-node root tail tail-offset))))
              (make-instance 'persistent-vector :root new-root :tail new-tail :tail-offset index :count (1+ count)))

            (make-instance 'persistent-vector :root root :tail (cons value tail) :tail-offset tail-offset :count (1+ count)))))))

(defmethod pop ((pv persistent-vector))
  (with-slots (root count tail-offset tail) pv
    (let ((new-tail (pop tail)))
      (if (and (> tail-offset 0) (= (count new-tail) 0))
          (let* ((new-tail-offset (max 0 (- tail-offset 32)))
                 (leaf-node (get-leaf-node root new-tail-offset))
                 (new-root (remove-leaf-node root new-tail-offset)))
            (make-instance 'persistent-vector
                           :root (if (= (count new-root) 1)
                                     (elt (slot-value new-root 'data) 0)
                                     new-root)
                           :tail leaf-node
                           :tail-offset new-tail-offset
                           :count (1- count)))
          (make-instance 'persistent-vector :root root :tail new-tail :tail-offset tail-offset :count (1- count))))))

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

(defmethod print-object ((object persistent-vector) stream)
  (let ((items (->list object)))
    (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
        (format stream "[~{~s~^ ~}]" items)
        (format stream "(persidastricl:persistent-vector ~{~s~^ ~})" items))))

(defmethod make-load-form ((object persistent-vector) &optional env)
  (declare (ignore env))
  (let ((items (->list object)))
    `(persidastricl::persistent-vector ,@items)))

(defmethod assoc ((pv persistent-vector) index item &rest kv-pairs)
  (cl:reduce
   (lambda (pv kv-pair)
     (with-slots (root tail count tail-offset) pv
       (destructuring-bind (idx item) kv-pair
         (cond
           ((= idx (count pv))
            (cons item pv))

           ((>= idx tail-offset)
            (make-instance (type-of pv) :root root :count count :tail-offset tail-offset :tail (add tail item :index idx)))

           (t
            (make-instance (type-of pv) :root (add root item :index idx) :count count :tail-offset tail-offset :tail tail))))))
   (->list (partition-all (list* index item kv-pairs) 2))
   :initial-value pv))

(defmethod update-instance-for-different-class :before ((old transient-vector)
                                                        (new persistent-vector)
                                                        &key)
  (slot-makunbound new 'root)
  (slot-makunbound new 'tail)
  (slot-makunbound new 'tail-offset)
  (slot-makunbound new 'count)

  (with-slots (root tail tail-offset count) old
    (setf (slot-value new 'root) (change-class root 'persistent-vector-node))
    (setf (slot-value new 'tail) (change-class tail 'persistent-vector-leaf-node))
    (setf (slot-value new 'tail-offset) tail-offset)
    (setf (slot-value new 'count) count)))
