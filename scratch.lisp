;; -----
;;  scratch.lisp
;;
;; test/explore/experiment/discover
;;
;; -----

(in-package :persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(defparameter m1 (with-meta
                   {:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4 :k5 :v5 :k6 :v6 :k7 :v7 :k1 #{1 2 3}}
                   {:a 1 :value "testing"}))
(get m1 :k1)
(get m1 :k1 :not-found)
(assoc m1 :k1 :derp)
(dissoc m1 :k1)
(get m1 :k7)
(get (meta m1) :a)
(get (meta m1) :value)

(meta m1)

(into [] (take 3 (lazy-seq '(1 2 3 4 5 6 7 8))))
(into #{} (take 8 (integers)))

(->vec m1)

(defparameter s1 @#{ 1 2 3 4 5 6 })
(contains? s1 3)

;; (setf hash::*default-hasher* hash::murmur128)
;; (setf b::*default-hash-slice-bit-size* 8)

(defvar m2 {:a 1
            :b "testing"
            :c {:a [1 2 3]
                :b #{1 2 3}
                :c {:a {:b {:c "testing"}}}}
            :d {:e (1 2 3 4)}
            :e {:f #(1 2 3 4)}
            :g {:h #{1 2 3 4}}})

;; these might be better as a walk
(->list m2)
(->alist m2)
(->vector m2)

(update-in m2 [:d :e 3] (fnil #'1+ 0))
(update-in m2 [:g :h] (fnil #'conj #{}) :NEW-VALUE 0 99)

;;
;; common lisp data structure compatibility
;;
(get '(0 1 2 3 4) 3)
(assoc '(0 1 2 3 4) 3 :three)

(get-in m2 [:d :e 0] :na)

(assoc m2 :b (string-upcase (get m2 :b)))

(time
 (setf v1 (update-in [:a] [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (fnil #'conj #{}) "test")))

(update [:a] 1 (fnil #'conj #{}) "test")

(time
 (get-in v1 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] :n))

(defun bitmap->bits (bitmap)
  (reduce
   (lambda (l pos)
     (acons (s:str "b" pos) (if (b:set? pos bitmap) "1" "0") l))
   (->list (range 32))
   :initial-value '()))

(bitmap->bits (random (expt 2 32)))

(defun bitmap->dot (bitmap)
  (s:join "|"  (map 'list  (lambda (c) (format nil "~a ~a" (first  c) (rest c))) (bitmap->bits bitmap))))

(bitmap->dot 3)

(defclass derp ()
  ((data :initarg :data :accessor :data)))

(defmacro doto (thing &body forms)
  (let ((obj (gensym)))
    `(let ((,obj ,thing))
       ,@(mapcar
          (lambda (f)
            (cond
              ((and (listp f)
                    (equal (first f) 'lambda)) `(,f ,obj))
              ((listp f) `(,(first f) ,obj ,@(rest f)))
              (t `(funcall ,f ,obj))
              ))
          forms)
       ,obj)))


(doto (into @[] (range 10))
      (conj 1 2 3)
      (conj :a :b :c)
      princ
      (lambda (tv) (princ (count tv))))



(defun slice (vector start &optional (end (count vector)))
  (lreduce
   (lambda (v i)
     (conj (get vector i) v))
   (range (- end start) :start start)
   :initial-value (empty vector)))

(defvar v (into #{} (take 10000 (integers))))

(time
 (subs v 10))

(function coqns)

(transient! (into [] (range 1000)))

(defvar pvln (make-instance 'n:persistent-vector-leaf-node))

(persistent->transient-name pvln)

(class-name (class-of pvln))

( (s:str "N:" (persistent->transient-name pvln)))

(find-class 'node:persistent-vector-leaf-node)

(persistent! @[])

(time
 (persistent! (into @[] (range 100000))))

(time
 (transient! (into [] (range 100000))))
