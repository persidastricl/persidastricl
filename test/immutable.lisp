;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/immutable.lisp
;;;
;;;  testing immutability in CLOS objects
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :immutable-tests
  :description "testing using the immutable-class as a metaclass for immutable objects used in persistent data structures"
  :in master-suite)

(in-suite :immutable-tests)

(define-immutable-class foo ()
  ((data :initarg :data :accessor :data)))

(test :using-setf-to-change-slot-values
  :description ""
  (let ((my-foo (make-instance 'foo :data 1)))
    (is (= 1 (:data my-foo)))
    (signals invalid-access-to-immutable-object (setf (:data my-foo) 2))
    (is (= 1 (:data my-foo)))
    (signals invalid-access-to-immutable-object (setf (slot-value my-foo 'data) 3))
    (is (= 1 (:data my-foo)))
    (with-slots (data) my-foo
      (signals invalid-access-to-immutable-object (setf data 4)))
    (is (= 1 (:data my-foo)))))


(defclass a-mixin ()
  ((other :initarg :other :accessor :other)))

(defclass baz (a-mixin) ())

(define-immutable-class bar (foo a-mixin)
  ((count :initarg :count :reader :count)))

(defmethod mixin-fn ((c a-mixin))
  (identity (:other c)))

(test :mixins-work-as-expected
  :description "using a mixin default to immutable with mixed into a mutable class; remains mutable otherwise"
  (let ((my-bar (make-instance 'bar :data 1 :other 1))
        (my-baz (make-instance 'baz :other 2)))
    (setf (:other my-baz) 10)
    (is (= 10 (mixin-fn my-baz)))
    (signals invalid-access-to-immutable-object (setf (:other my-bar) 10))))

;; (fiveam:run! :immutable-tests)
