;; -----
;;  scratch.lisp
;;
;; test/explore/experiment/discover
;;
;; -----

(in-package :persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(defparameter m1 (with-meta
                   @{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4 :k5 :v5 :k6 :v6 :k7 :v7 :k1 #{1 2 3}}
                   {:a 1 :value "testing"}))
(get m1 :k1)
(get m1 :k1 :not-found)
(assoc m1 :k1 :derp)
(dissoc m1 :k1 :k2 :k3 :k4 :k5 :k6)
(get m1 :k7)
(get (meta m1) :a)
(get (meta m1) :value)

(meta m1)
(->vec m1)
(->alist m1)


(->list [1 2 3])

(type-of fib)

(bounded-count 100 fib)

(empty? fib)

(into @{} (take 10 fib))

(into #{} (take 8 (integers)))

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

(get-in m2 [:c :c])
(get-in m2 [:d :e 0] :na)

(-> m2
  (update-in [:d :e 3] (fnil #'1+ 0))
  (update-in [:g :h] (fnil #'conj #{}) :NEW-VALUE 0 99))

(get m2 :b)
(-> m2
  (assoc :b (string-upcase (get m2 :b)))
  (get :b))



(defvar m3 {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})

(seq m3)

(->list m3)
(->alist m3)
(->vector m3)

;;
;; common lisp data structure compatibility
;;
(get '(0 1 2 3 4) 3)
(assoc '(0 1 2 3 4) 3 :three)

(time
 (setf v1 (update-in [:a] [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] (fnil #'conj #{}) "test")))

(time
 (get-in v1 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] :n))

(update [:a] 1 (fnil #'conj #{}) "test")


;; -- output structures to graphiz?? --

(defun bitmap->bits (bitmap)
  (lreduce
   (lambda (l pos)
     (acons (s:str "b" pos) (if (b:set? pos bitmap) "1" "0") l))
   (range 32)
   :initial-value '()))

(bitmap->bits (random (expt 2 32)))

(defun bitmap->dot (bitmap)
  (s:join "|" (->list  (lmap (lambda (c) (format nil "~a ~a" (first  c) (rest c))) (bitmap->bits bitmap)))))

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
  (lambda (tv) (princ (count tv))))

(take 10 '(1 2 3 4 5 6 nil nil nil))

(every? #'oddp '(1 3 5 7 9 11 13))
(every? #'oddp '(1 3 5 7 9 12 13))

(into [] (lmap (lambda (i) i) '(1 2 3 4 5 6 nil nil nil)))
(into [] (partition-all '(1 2 3 4 5 6 7 8 9 0 1) 2))
(into [] (partition '(1 2 3 4 5 6 7) 2))

(reduce-kv
 (lambda (r k v)
   (assoc r v k))
 {:a 1 :b 2 :c 3}
 :initial-value {})

(into [] (take-last 100 (range 1000)))

(into [] (drop-last 100 (range 150 :start 1000)))

;; (defun slice (vector start &optional (end (count vector)))
;;   (lreduce
;;    (lambda (v i)
;;      (conj (get vector i) v))
;;    (range (- end start) :start start)
;;    :initial-value (empty vector)))


(defvar lowercase-letters
  (lmap
   (lambda (i)
     (code-char i))
   (range 26 :start 97)))

(defvar uppercase-letters
  (lmap
   (lambda (i)
     (code-char i))
   (range 26 :start 65)))

(interleave lowercase-letters uppercase-letters)

(defvar my-map
  (into {} (interleave uppercase-letters (integers))))

(->keys my-map)
(->vals my-map)

(bounded-count 100 '(1))

(sb-sys:without-gcing (time (take 1 (drop 10000000 (integers)))))
