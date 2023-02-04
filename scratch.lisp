
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

