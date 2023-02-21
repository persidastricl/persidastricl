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

(->list [1 2 3])

(into [] (take 3 (lazy-seq '(1 2 3 4 5 6 7 8))))

(print-object
 nil)

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

(sort '("b" "z" "a") #'string<)

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

(sb-ext:gc)
(room)

(require :sb-sprof)

(sb-sprof:start-profiling )

(take 1 (sb-sys:without-gcing (time (drop 10000000 (integers)))))

(time (take 1 (drop 10000000 (integers))))

(sb-sys:without-gcing (time (take 1 (drop 10000000 (integers)))))


(sb-sys:without-gcing
  (time
   (let ((r (take 1 (drop 10000000 ints))))
     r)))

(sb-sprof:with-profiling ()

  (drop 10000000 (integers)))

(defvar ints (integers))

(sb-sprof:report)

(sb-sprof:stop-profiling)

(sort (->keys m) #'char>)

(setf m (into {} (interleave uppercase-letters (integers))))
(->keys m)
(->vals m)

(->vals (get-in m2 [:c :c]))
(into [] (->vals m2))

(setf *print-lazy-items* 3)

(seq m2)

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


(zipmap [:a :b :c] [1 2 3 4])

(->list  (take 2 '(1)))

(into [] (partition-all '(1 2 3 4 5 6 7 8 9 0 1) 2))
(into [] (partition '(1 2 3 4 5 6 7) 2))

(head (seq {:a 1 :b 2}))
(reduce-kv
 (lambda (r k v)
   (assoc r v k))
 {:a 1 :b 2 :c 3}
 :initial-value {})


(code-char 97)

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

(defvar pvln (make-instance 'persistent-vector-leaf-node))

(persistent->transient-name pvln)

(class-name (class-of pvln))

(find-class 'node:persistent-vector-leaf-node)

(persistent! @[])

(time
 (setf v1  (into @[] (range 1000000))))

(time (persistent! v1))

(time
 (persistent! (into @[] (range 1000000))))

(time
 (into [] (range 1000000)))

(time
 (transient! (into [] (range 1000000))))


(with-open-file (s "/home/mdp/Documents/favorites/BitsAndPieces.txt" :external-format :iso-8859-2 )
  (s:join (s:str #\NEWLINE) (->list  (lmap
                                      (lambda (line)
                                        (string-upcase (s:trim line)))
                                      (line-seq s)))))

(step)
(count #{1 2 3 4})


(has-no-value? #())


(-> {}
  (assoc :a nil))

(only-valid-values? (persistent-hash-map :a #{} :b 1))


(defvar test-node (make-instance 'persistent-hash-set-node))

(-> test-node
  type-of
  s:str)
(empty-overflow-node test-node)
