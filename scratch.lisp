
(in-package :persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(defparameter m1 (with-meta
                   {:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4 :k5 :v5 :k6 :v6 :k7 :v7 :k1 #{1 2 3}}
                   {:a 1 :value "testing"}))
(lookup m1 :k1)
(lookup m1 :k7)
(lookup (meta m1) :a)
(lookup (meta m1) :value)

(->alist m1)

(defparameter s1 #{ 1 2 3 4 5 6 })
(contains? s1 3)



(setf hash::*default-hasher* hash::murmur32)
(setf m10 (transient-hash-map))
(setf m10 (assoc m10 :k1 :v1 :k2 :v2))

(setf s10 (transient-hash-set))
(setf s10 (conj s10 :a :b :c :d :e :f))

(->alist m10)
(seq s10)

(contains? s10 'd)

(setf s1 (elt (sub-nodes (:root m10)) 0))

(setf o1 (elt (sub-nodes s1) 0))

(count o1)

(setf oit  (iterator o1))

(next oit)

(current it)

(setf it  (iterator m10))

(has-next? it)

(next it)

(setf s10 (persistent-hash-set))
(setf s10 (conj s10 :k1 :k2))

(contains? s10 :k3)

(setf s11 (disj s10 :k2))

(lookup m10 :k1)
(lookup m11 :k1)

(setf m11 (dissoc m10 :k1))
(lookup m11 :k2)

(time
 (setf m10 (apply #'assoc m10 (flatten (loop for i from 0 upto 1000
                                             collect (list (->keyword (s:str "k"  i)) (s:str "v" i)))))))


(time
 (setf m10 (into m10 (flatten (loop for i from 0 upto 1000
                                    collect (list (->keyword (s:str "k"  i)) (s:str "v" i)))))))

(setf m10 (apply #'dissoc m10  (loop for i from 2 upto 1000
                                     collect  (->keyword (s:str "k"  i)))))

(->alist m10)

(time
 (lookup m10 :k1))


(map 'list  (lambda (k) (h:hash k)) (flatten (loop for i from 0 upto 1000
                                                   collect (list  (s:str 'k  i) (s:str 'v i)))))
(setf m10 (conj m10 1 2 3 4 5 6 7))



(symbol-name  (find-class  (type-of tra)))


(defmethod ->persistent ((original transient))
  (generic-labels ((convert ((node transient-overflow-node))
                     (with-slots (hash values) node
                       (make-instance (persistent-sister-class node) :hash hash :values values)))
                   (convert ((node node))
                     (with-slots ())))))

(setf per (->per tra))




;; -----
;;  lazy sequences/deferred evaluations
;;
;; -----

(defclass thunk ()
  ((fn :initarg :fn :accessor :fn)
   (r  :initarg :r  :accessor :r))
  (:default-initargs :r nil))

(defmacro delay (&rest body)
  `(make-instance 'thunk :fn (lambda () ,@body)))

(defgeneric force (obj)
  (:method (obj) obj))

(defmethod force ((thunk thunk))
  (if (and (slot-boundp thunk 'fn)
           (functionp (:fn thunk)))
      (let ((values (multiple-value-list (funcall (:fn thunk)))))
        (setf (:r thunk) values)
        (slot-makunbound thunk 'fn)))
  (values-list  (:r thunk)))

(defmacro lcons (a b)
  `(cl:cons ,a (delay ,b)))

(defmethod lcar (lcons)
  (car lcons))

(defmethod lcdr (lcons)
  (force (cl:cdr lcons)))

(defmethod seq ((phs persistent-hash-set))
  (let ((it (iterator phs)))
    (labels ((next-entry ()
               (when (has-next? it)
                 (lcons (next it) (next-entry)))))
      (next-entry))))

(defun _reduce (f s &key (initial-value nil initial-arg-p))
  (labels ((reduce* (seq acc)
             (if seq
                 (let ((current (lcar seq)))
                   (reduce* (lcdr seq) (funcall f acc current)))
                 acc)))
    (reduce* (ensure-seq s) initial-value)))

(defun integers (&key (from 0))
  (lcons from (integers :from (1+ from))))

(defun range (n &key (start 0) (step 1))
  (when (> n 0)
    (lcons start (range (1- n) :start (+ start step) :step step))))

(defun ensure-seq (s)
  (if (consp s) s (seq s)))

(defun take (n seq)
  (labels ((take* (n seq acc)
             (if (or (<= n 0) (null seq))
                 acc
                 (let ((v (lcar seq)))
                   (take* (1- n) (lcdr seq) (conj acc v))))))
    (take* n (ensure-seq seq) (persistent-vector))))

(defun drop (n seq)
  (labels ((drop* (n seq)
             (if (and (consp seq) (> n 0))
                 (drop* (1- n) (lcdr seq))
                 seq)))
    (drop* n (ensure-seq seq))))

(defun filter (pred seq)
  (labels ((filter* (seq acc)
             (if seq
                 (let ((v (lcar seq)))
                   (if (funcall pred v)
                       (filter* (lcdr seq) (conj acc v))
                       (filter* (lcdr seq) acc)))
                 acc)))
    (filter* (ensure-seq seq) (persistent-vector))))

(defun _map (f &rest seqs)
  (if (emptyp seqs)
      nil
      (let ((n (length seqs))
            (seqs (map 'list #'ensure-seq seqs)))
        (labels ((apply* (args)
                   (cl:apply f args))
                 (map* (s)
                   (let ((args (remove-if #'null (map 'list #'lcar s))))
                     (when (= n (length args))
                       (let ((r (apply* args)))
                         (lcons r (map* (cl:map 'list #'lcdr s))))))))
          (map* seqs)))))




(take 10 fib)
(setf fib100  (take 10000 fib))

(time
 (_reduce #'+ fib100 :initial-value 0))

(count fib100)

(+ 1 1)

(take 10 )

(take 10 (filter #'oddp (take 1000 (drop 5000000 (range 10000000000000 :start 1000000000)))))

(defvar integers )


(defun fib ()
  ())


(setf v1  [1 2 3])
