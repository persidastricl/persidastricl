;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   iterator.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass iterator () ())

(defmethod iterator (target))
(defgeneric has-next? (object))
(defgeneric next (object))


(defclass hash-map-node-iterator (iterator)
  ((node :initarg :node :accessor :node)
   (index :initarg :index :accessor :index)))

(defmethod iterator ((target hash-map-node))
  (make-instance 'hash-map-node-iterator :node target :index 0))

(defmethod has-next? ((iterator hash-map-node-iterator))
  (with-slots (index node) iterator
    (< index (count (:dmap node)))))

(defmethod next ((iterator hash-map-node-iterator))
  (with-slots (node index) iterator
    (when (< index (count (:dmap node)))
      (let ((entry (at-index (:dmap node) index)))
        (incf index)
        entry))))


(defclass hash-map-iterator (iterator)
  ((stack :initarg :stack :accessor :stack)
   (current :initarg :current :accessor :current)))

(defmethod iterator ((target persistent-hash-map))
  (make-instance 'hash-map-iterator :stack (coerce (:data (:nmap (:root  target))) 'list) :current (iterator (:root target))))

(defmethod has-next? ((iterator hash-map-iterator))
  (has-next? (:current iterator)))

(first '())

(defun next-iterator (stack)
  (if (emptyp stack)
      (values nil nil)
      (let ((node (cl:first stack)))
        (if (> (count (:dmap node)) 0)
            (values (iterator node) (concatenate 'list (:data (:nmap node)) (cl:rest stack)))
            (next-iterator (concatenate 'list (:data (:nmap node)) (cl:rest stack)))))))

(defmethod next ((iterator hash-map-iterator))
  (with-slots (current stack) iterator
    (let ((entry (next current)))
      (when-not (has-next? current)
        (multiple-value-bind (next-current new-stack) (next-iterator stack)
          (when next-current
            (setf current next-current)
            (setf stack new-stack))))
      entry)))

(defparameter phm (-> (make-instance 'persistent-hash-map)
                    (assoc :k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4 :k5 :v5 :k6 :v6 :k7 :v7)))

(defun print-entries (phm)
  (labels ((print-next (it)
             (when (has-next? it)
               (let ((entry (next it)))
                 (format t "~S ~S~%" (e:key entry) (e:value entry))
                 (print-next it)))))
    (print-next (iterator phm))))

(print-entries phm)

(has-next? it)
(next it)

(setf e *)
