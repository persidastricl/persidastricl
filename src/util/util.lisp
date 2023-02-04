;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   util.lisp
;;;
;;; -----

(in-package #:persidastricl)

(proclaim '(inline last-one single-p append-one conc-one ensure-list))

(defun emptyp (sequence)
  (typecase sequence
    (null t)
    (array (zerop (length sequence)))
    (otherwise nil)))

(defmacro while (condition &rest body)
  `(loop while ,condition
         do (progn ,@body)))

(defmacro if-not (pred truthy &optional falsey)
  `(if (not ,pred) ,truthy ,falsey ))

(defmacro when-not (pred &body body)
  `(when (not ,pred)
     ,@body))

(defmacro if-let ((var test-form) then-form &optional else-form)
  `(let ((,var ,test-form))
     (if ,var ,then-form ,else-form)))

(defmacro when-let ((var test-form) &body body)
  `(let ((,var ,test-form))
     (when ,var
       ,@body)))

(defun last-one (lst)
  "return the last item in a list"
  (first (last lst)))

(defun single-p (lst)
  "is this a single item list?"
  (and (consp lst) (not (rest lst))))

(defun append-one (lst obj)
  "append an object to the list"
  (append lst (list obj)))

(defun conc-one (lst obj)
  "append an object to the list destructively"
  (nconc lst (list obj)))

(defun ensure-list (obj)
  "ensure object is indeed a list"
  (if (listp obj)
      obj
      (list obj)))

(defun longer-p (x y)
  "is list x longer than list y"
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (rest x) (rest y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun keep (pred lst)
  "apply pred to list and keep results if satisfied"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall pred x)))
        (when val
          (push val acc))))
    (nreverse acc)))

(defun partition (source n)
  "return list with source divided into lists of n items plus final list (< n) of remainders"
  (when (zerop n) (error "partition is zero length"))
  (labels ((rec (source acc)
             (let ((more (nthcdr n source)))
               (if (consp more)
                   (rec more (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "return list of all leaf nodes in x"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (first x) (rec (rest x) acc))))))
    (rec x nil)))

(defun prune (pred tree)
  "remove all leaf nodes in tree that satisfy pred"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (first tree)) (rec (rest tree) (cons (rec (first tree) nil) acc)))
                   (t (rec (rest tree) (if (funcall pred (first tree))
                                           acc
                                           (cons (first tree) acc)))))))
    (rec tree nil)))

(defun find-first (pred lst)
  "return the values of first occurence of element in list that satisfies pred, and its result"
  (when lst
    (let ((result (funcall pred (first lst))))
      (if result
          (values (first lst) result)
          (find-first pred (rest lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((target (first lst)))
         (cond ((funcall test y target) nil)
               ((funcall test x target) lst)
               (t (before x y (rest lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((target (before y x lst :test test)))
    (and target (member x target :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (let ((sub (rest (member obj lst :test test))))
    (member obj sub :test test)))

(defun split-by (pred lst)
  (let ((acc nil))
    (do ((src lst (rest src)))
        ((or (null src) (funcall pred (first src)))
         (values (nreverse acc) src))
      (push (first src) acc))))

(defun most (fn lst)
  (declare (optimize debug))
  (if (null lst)
      (values nil nil)
      (let* ((wins (first lst))
             (max (funcall fn wins)))
        (dolist (obj (rest lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))
