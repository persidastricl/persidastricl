(+ 11)

(defun bitmap (node)
  (car node))

(defun data (node)
  (cadr node))

(defun test-fn (node)
  (princ (bitmap node))
  (princ (data node)))

(test-fn '(1001 #(1 2 3)))

(sb-ext:primitive-object-size 1)

( '(1 2))

sb-vm:single-float-size

sb-vm:large-object-size

(setf bvn (make-instance 'persistent-key-value-bitmap-vector :bitmap 1001 :data #(1 2 3)))

(+)
(sb-ext:primitive-object-size #(1001 #(1 2 3)))

(sb-ext:primitive-object-size '(1001 #(1 2 3)))

(sb-ext:primitive-object-size #())

(in-package :persidastricl)
(setf phm (persistent-hash-map :a 1 :b 2))

(sb-vm:hexdump (:nmap  (:root  phm)))

(hcl:find-object-sizeqq)

(disassemble 'empty-transient-key-value-bitmap-vector)

(defgeneric insert (into-this at-position this-item &rest args))

;;
;; THIS!!!!! will work for the different ways we call things like insert/update/get/put/assoc/conj etc
;;

(defgeneric _put (into-this item &rest args))

(defmethod _put ((v array) item &key hash level &allow-other-keys)
  (list* v item hash level))

(defmethod _put ((v string) item &key level &allow-other-keys)
  (list* v item level))

(defmethod _put ((vec array) item &key hash level &allow-other-keys)
  (list* vec item hash level))

(defmethod _put ((v list) item &rest args)
  (list* v  args))

(_put #(1 2 3) 4 :hash 18 :level 2)
(_put '(1 2 3) 4)
(_put "test" 1 :level 1 :hash nil)
(_put '(1 2 3) 4 3 2 1)

(defgeneric _get (obj key &optional default &rest args))

(defmethod _get (obj key &optional (default nil) &key hash level))

(defmethod _get (obj key &optional (default nil) &key))


(v:insert )
(defmethod conj ((v array) &rest items))

(defgeneric count)

(defmethod test-method ((object1 persistent-bitmap-vector) item index &keys (hash 0) &allow-other-keys (depth 0)))
