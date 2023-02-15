;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; transient-vector.lisp
;;;
;;; -----

(in-package :persidastricl)

(defclass transient-vector (vector) ()
  (:default-initargs :root (make-instance 'n:transient-vector-node) :tail (make-instance 'n:transient-vector-leaf-node) :tail-offset 0))

(defmethod cons (value (tv transient-vector))
  (labels ((new-root? (node)
             (with-slots (n::data n::level) node
               (and (= (n:count n::data) 32)
                    (if (> n::level 1)
                        (new-root? (elt n::data 31))
                        t)))))
    (with-slots (root count tail-offset tail) tv
      (let ((index count)
            (new-tail? (= (n:count tail) 32)))

        (if new-tail?
            (let* ((new-tail (n:cons value (make-instance 'n:transient-vector-leaf-node)))
                   (new-root (if (new-root? root)
                                 (let ((nr (make-instance 'n:transient-vector-node :level (1+ (:level root)))))
                                   (with-slots (n::data) nr
                                     (vector-push root n::data))
                                   (n:add-leaf-node nr tail tail-offset))
                                 (n:add-leaf-node root tail tail-offset))))
              (setf root new-root
                    tail new-tail
                    tail-offset index))
            ;; no need for a new tail
            (n:cons value tail))

        (setf count (1+ count)))))
  tv)

(defgeneric t-vec (object)
  (:method (obj) (into (transient-vector) (seq obj))))

(defun transient-vector (&rest items)
  (reduce
   (lambda (tv item)
     (cons item tv))
   items
   :initial-value (make-instance 'transient-vector)))

(defmethod print-object ((obj transient-vector) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "@[~{~s~^ ~}]" (->list obj))
      (format stream "(persidastricl:transient-vector  ~{~s~^ ~})" (->list obj))))

(defmethod make-load-form ((obj transient-vector) &optional env)
  (declare (ignore env))
  (let ((items (flatten (seq obj))))
    `(persidastricl::transient-vector ,@items)))

(defmethod assoc ((tv transient-vector) index item &rest kv-pairs)
  (with-slots (root tail tail-offset) tv
    (reduce
     (lambda (tv kv-pair)
       (destructuring-bind (idx item) kv-pair
         (if (>= idx tail-offset)
             (setf tail (n:put tail item idx))
             (setf root (n:put root item idx)))
         tv))
     (->list (partition (list* index item kv-pairs) 2))
     :initial-value tv)))
