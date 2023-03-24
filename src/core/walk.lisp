;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; walk.lisp
;;;
;;; -----

(in-package :walk)

(named-readtables:in-readtable persidastricl:syntax)

(defgeneric walk (inner outer form)
  (:method (inner outer form) (funcall outer form)))

(defmethod walk (inner outer (form list))
  (funcall outer (into (empty form) (map inner form))))

(defmethod walk (inner outer (entry p::entry))
  (funcall outer (p::map-entry (funcall inner (p::key entry)) (funcall inner (p::value entry)))))

(defmethod walk (inner outer (m p::hash-map))
  (funcall outer (into (empty m) (map inner m))))

(defmethod walk (inner outer (collection p::collection))
  (funcall outer (into (empty collection) (map inner collection))))

(defun postwalk (f form)
  (walk
   (partial #'postwalk f)
   f
   form))

(defun prewalk (f form)
  (walk
   (partial #'prewalk f)
   #'identity
   (funcall f form)))

(defun keywordize-keys (m)
  (labels ((keywordize-entry (e)
             (let ((k (p::key e))
                   (v (p::value e)))
               (if (string? k)
                   (p::map-entry (keyword k) v)
                   e))))
    (postwalk (lambda (x) (if (map? x) (into {} (map #'keywordize-entry x)) x)) m)))

;; TODO: fix case here
(defun stringify-keys (m)
  (labels ((symbol-name* (k)
             (let ((s (symbol-name k)))
               (cond
                 ((every (complement #'upper-case-p) s) (string-upcase s))
                 ((every (complement #'lower-case-p) s) (string-downcase s))
                 (t s))))
           (stringify-entry (e)
             (let ((k (p::key e))
                   (v (p::value e)))
               (if (keywordp k)
                   (p::map-entry (symbol-name* k) v)
                   e))))
    (postwalk (lambda (x) (if (map? x) (into {} (map #'stringify-entry x)) x)) m)))

(defun prewalk-demo (form)
  (prewalk (lambda (x) (princ "Walked: ") (princ (format nil "~s~%" x)) x) form))

(defun postwalk-demo (form)
  (postwalk (lambda (x) (princ "Walked: ") (princ (format nil "~s~%" x)) x) form))

(defun prewalk-replace (smap form)
  (prewalk (lambda (x) (let ((v (get smap x))) (or v x))) form))

(defun postwalk-replace (smap form)
  (postwalk (lambda (x) (let ((v (get smap x))) (or v x))) form))

(defun macroexpand-all (form)
  (prewalk (lambda (x) (if (typep x 'list) (macroexpand x) x)) form))

;; -----
;; examples
;;
;; -----

;; (walk (partial #'* 2)  (lambda (x) (apply #'+ (->list x))) (set [1 2 3 4 5]))
;; (walk (partial #'* 2)  (lambda (x) (apply #'+ (->list x))) [1 2 3 4 5])
;; (walk #'first (lambda (x) (reverse (->list x))) [ [1 2] [3 4] [5 6] ])

;; (walk (lambda (e)
;;         (apply #'p::map-entry (->list (map #'str e))))
;;       (lambda (form)
;;         form)
;;       {:a 1})

;; (keywordize-keys {"a" 1})
;; (keywordize-keys {"a" 1 "b" 2 "c" {"d" [1 2 3 3] "e" 4 "f" #{1 2 3}}})

;; (stringify-keys {:a 1})
;;(stringify-keys {:A 1 :C {:D [1 2 3 3] :E 4 :Eagle 5 :abCD 6 :F #{1 2 3}} :B 2 :a 7 :b 3})



;; (prewalk-demo {"a" 1 "b" 2 "c" {"d" [1 2 3 3] "e" 4 "f" #{1 2 3}}})
;; (postwalk-demo {"a" 1 "b" 2 "c" {"d" [1 2 3 3] "e" 4 "f" #{1 2 3}}})

;; (prewalk-replace {:a 1 :b 2} [:a :b :c])
;; (prewalk-replace {:a 1 :b 2 :c 7} [:a :b :c])
;; (postwalk-replace {:a 1 :b 2 :c 7} [:a :b [:a :b] :c :d])


;; (macroexpand-all '(cond
;;                    ((= a 1) 2)
;;                    ((= a 2)  (inc a))
;;                    (t (+ 6 6))))

;; (macroexpand-all  '(arrow-macros:-> (* 3 2)
;;                     (+ 5)
;;                     (* 6)))
