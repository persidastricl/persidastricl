;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector.lisp
;;;
;;; -----

(in-package :persidastricl)

(define-immutable-class persistent-vector (bpvt) ()
  (:default-initargs :root (make-instance 'n:persistent-vector-node) :tail (make-instance 'n:persistent-vector-tail-node) :tail-offset 0))

(defmethod cons (value (pv persistent-vector))
  (labels ((new-root? (node)
             (with-slots (n::data n::level) node
               (and (== (n:count n::data) 32)
                    (if (> n::level 1)
                        (new-root? (elt n::data 31))
                        t)))))
    (with-slots (root count tail-offset tail) pv
      (let ((index count)
            (new-tail? (== (n:count tail) 32)))

        (if new-tail?
            (let* ((new-tail (n:cons value (make-instance 'n:persistent-vector-tail-node)))
                   (new-root (if (new-root? root)
                                 (let ((nr (make-instance 'n:persistent-vector-node :data (make-array 1 :initial-element root)
                                                                                    :level (1+ (:level root)))))
                                   (n:add-leaf-node nr (change-class tail 'n:persistent-vector-leaf-node) tail-offset))
                                 (n:add-leaf-node root (change-class tail 'n:persistent-vector-leaf-node) tail-offset))))
              (make-instance 'persistent-vector :root new-root :tail new-tail :tail-offset index :count (1+ count)))

            (make-instance 'persistent-vector :root root :tail (n:cons value tail) :tail-offset tail-offset :count (1+ count)))))))

(defmethod get ((pv persistent-vector) index &optional (default nil))
  (if (< index (count pv))
      (if (>= index (:tail-offset pv))
          (n:get (:tail pv) index (list default))
          (n:get (:root pv) index (list default)))
      default))

(defmethod seq ((pv persistent-vector))
  (labels ((next (i)
             (when-let (v (get pv i))
               (lazy-seq v (next (1+ i))))))
    (next 0)))

(defmethod ->list ((pv persistent-vector))
  (map 'list (lambda (i) (get pv i)) (loop for i from 0 below (count pv) collect i)))

(defgeneric vec (object)
  (:method (obj) (into (persistent-vector) (seq obj))))

(defun persistent-vector (&rest items)
  (reduce
   (lambda (pv item)
     (cons item pv))
   items
   :initial-value (make-instance 'persistent-vector)))

(defmethod print-object ((object persistent-vector) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "[~{~s~^ ~}]" (->list object))
      (format stream "(persidastricl:persistent-vector  ~{~s~^ ~})" (->list object))))

(defmethod make-load-form ((obj persistent-vector) &optional env)
  (declare (ignore env))
  (let ((items (flatten (seq obj))))
    `(persidastricl::persistent-vector ,@items)))

(defmethod conj ((pv persistent-vector) &rest values)
  (reduce
   #'(lambda (v value)
       (cons value v))
   values
   :initial-value pv))
