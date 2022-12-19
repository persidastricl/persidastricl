;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   scratch.lisp
;;;
;;; explorations in hamt data nodes/bit manipulations/etc
;;;
;;; -----

;;; TODO clean up

;;;; put phm in it's own file
;;;; clean up names of things
;;;; associable?   generic functions assoc, dissoc, get/lookup

(defclass persistent-hash-map (metadata:metadata seqable:seqable counted:counted)
  ((root :type 'node:node :initarg :root :reader :root :documentation "root node of hash-map"))
  (:default-initargs :root node::empty :meta nil :count 0))

;; TODO: fix `count` on update
(defmethod put ((m persistent-hash-map) key value)
  (with-slots (root m::meta c::count) m
    (let* ((hash (h:hash key))
           (entry (e:entry key value))
           (context (list hash 0)))
      (make-instance 'persistent-hash-map :root (node:put root entry context) :meta m::meta :count (1+ c:count)))))

(defmethod _get ((m persistent-hash-map) key &optional default)
  (with-slots (root) m
    (let* ((hash (h:hash key)))
      (node:get root key (list hash 0 default)))))


(defmethod dissoc ((m persistent-hash-map) key &optional (default :not-found))
  (with-slots (root m::meta c::count) m
    (let* ((hash (h:hash key)))
      (make-instance 'persistent-hash-map :root (node:delete root key (list hash 0 default)) :meta m::meta :count (1- c::count)))))
( )

(setf m (make-instance 'persistent-hash-map))

(elt  (e:entry "test" "value") 0)

(setf m (put m :k6 "v6"))

(setf m (put m :k5 "v5"))

(_get m :k6 :NOT-found)
(_get m :k5 :NOT-found)

(setf m2 (put m :k3 "v3"))

(setf m3 (put m2 :k3 "v3a"))

(_get m3 :k3)
(_get m3 :k6)
(_get m3 :k5)

(setf m4 (dissoc m3 :k5))

(_get m4 :k3)
(_get m4 :k6)
(_get m4 :k5)


(cl-murmurhash:murmurhash "test")
