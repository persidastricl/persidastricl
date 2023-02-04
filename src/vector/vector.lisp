;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; vector.lisp
;;;
;;; -----

(in-package :persidastricl)

(defparameter *items-per-node* 32)

;; -----
;;  vector-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass vector-node ()
  ((level :initarg :level :reader :level)
   (data :initarg :data :reader :data))
  (:default-initargs :data (make-array 0) :level 1))

(define-immutable-class persistent-vector-node (vector-node) ())

;; get walks nodes based on level ... recursive to level indexed node node (returns value)

(defmethod get-it ((node vector-node) index context)
  (let ((i (b:bits index (:level node))))
    (get-it (elt (:data node) i) index context)))

;; assoc ;; changes the value at index (returns new root)

;; add new leaf node (full tail node) with given level one index (returns new root)
;;    use change-class to chane class of full tail node to leaf node

(defun add-leaf-node (node leaf first-index-of-values)
  (with-slots (level data) node
    (let* ((i (b:bits first-index-of-values (:level node))))
      (labels ((sub-node (i)
                 (when (< i (length data))
                   (elt data i))))
        (if (> level 1)
            ;; node of sub-nodes
            (if-let (sub-node (sub-node i))
              ;; have subnode for this level
              (make-instance (type-of node) :level level :data (v:update data i (add-leaf-node sub-node leaf first-index-of-values)))
              ;; no sub-node for this level (need to add one)
              (let ((new-sub-node (make-instance (type-of node) :level (1- level))))
                (assert (= i (length data)))
                (make-instance (type-of node) :level level :data (v:append data (add-leaf-node new-sub-node leaf first-index-of-values)))))
            ;; level 1 node
            (progn
              (assert (== i (length data)))
              (make-instance (type-of node) :level level :data (v:append data leaf))))))))

;; remove leaf node with level 1 index (returns values of new root, removed leaf node to be new tail on 'pop')

(define-immutable-class persistent-vector-leaf-node (persistent-vector-node) ()
  (:default-initargs :level 0))

;; get value at level 0 index

(defmethod get-it ((node persistent-vector-leaf-node) index context)
  (destructuring-bind (default) context
    (let* ((i (b:bits index 0))
           (v (elt (:data node) i)))
      (or v default))))

(define-immutable-class persistent-vector-tail (persistent-vector-leaf-node) ())

;; cons

(defmethod cons :before (item (node persistent-vector-tail))
  ;; cannot add more items than we have bits in the hash
  (assert (< (length (:data node)) *items-per-node*)))

(defmethod cons (item (node persistent-vector-tail))
  (make-instance (type-of node) :level 0 :data (v:append (:data node) item)))




(define-immutable-class persistent-vector (counted collection seqable)
  ((root :initarg :root :reader :root)
   (tail :initarg :tail :reader :tail)
   (tail-offset :initarg :tail-offset :reader :tail-offset))
  (:default-initargs :root (make-instance 'persistent-vector-node) :tail (make-instance 'persistent-vector-tail) :tail-offset 0))


(defun new-root? (node)
  (with-slots (data level) node
    (and (== (length data) 32)
         (if (> level 1)
             (new-root? (elt data 31))
             t))))

(defmethod cons (value (pv persistent-vector))
  (with-slots (root count tail-offset tail) pv
    (let ((index count)
          (new-tail? (== (length (:data tail)) 32)))

      (if new-tail?
          (let* ((new-tail (cons value (make-instance 'persistent-vector-tail)))
                 (new-root (if (new-root? root)
                               (let ((nr (make-instance 'persistent-vector-node :data (make-array 1 :initial-element root) :level (1+ (:level root)))))
                                 (add-leaf-node nr (change-class tail 'persistent-vector-leaf-node) tail-offset))
                               (add-leaf-node root (change-class tail 'persistent-vector-leaf-node) tail-offset))))
            (make-instance 'persistent-vector :root new-root :tail new-tail :tail-offset index :count (1+ count)))

          (make-instance 'persistent-vector :root root :tail (cons value tail) :tail-offset tail-offset :count (1+ count))))))

(defmethod get ((pv persistent-vector) index &optional (default nil))
  (if (< index (:count pv))
      (if (>= index (:tail-offset pv))
          (get-it (:tail pv) index (list default))
          (get-it (:root pv) index (list default)))
      default))

(defmethod seq ((pv persistent-vector))
  (labels ((next (i)
             (when-let (v (get pv i))
               (lazy-seq v (next (1+ i))))))
    (next 0)))

(defmethod ->list ((pv persistent-vector))
  (map 'list (lambda (i) (get pv i)) (loop for i from 0 below (:count pv) collect i)))

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


;; (defun target-leaf-node (pv index)
;;   (with-slots (partitioner level count root tail) pv
;;     (labels ((get* (node level)
;;                (let* ((offset (bit:bits partitioner level index))
;;                       (item (elt (slot-value  node 'data) offset)))
;;                  (if (is-node-p item)
;;                      (get* item (1- level))
;;                      node))))

;;       (when (>= index count) (error "index out of bounds"))
;;       (get* root level))))


;; (defmethod get ((pv persistentvector) index &optional (default nil))
;;   (with-slots (partitioner level count root tail) pv
;;     (labels ((get* (node level)
;;                (let* ((offset (bit:bits partitioner level index))
;;                       (item (elt (slot-value  node 'data) offset)))
;;                  (if (is-node-p item)
;;                      (get* item (1- level))
;;                      item))))

;;       (when (>= index count) (error "index out of bounds"))
;;       (get* root level))))


;; (defmethod assoc :before ((pv PersistentVector) position value)
;;   (check-type position Integer))

;; (defmethod assoc ((pv PersistentVector) position value)
;;   (with-slots (level count root partitioner) pv
;;     (let* ((tail-index (bit:bits partitioner 0 position))
;;            (target (target-leaf-node pv position))
;;            (new-tail (assoc target tail-index value)))
;;       (make-persistent-vector level count (persist partitioner level position root target new-tail) new-tail))))
