;; -----
;;
;; -*- mode: Lisp; -*-
;;
;;  user-example.lisp
;;

;; load up the persidastricl library
(ql:quickload :persidastricl)

;; there is a :user package that has already set things up for you if you want to just explore
(in-package #:user)

;; turn on the syntactic sugar
(named-readtables:in-readtable persidastricl:syntax)

;; do something (same code from example.lisp)

(def m (with-meta {:A 0 :a 1 :b 2 :c 3 :d {:e #{1 2 3 4 5} :f [1 2 3 4 5] :g '(9 8 7 6 5)}} {:data {:start 100 :end 200}}))

(meta m)

;; some convenience hacks (not sure this is a good idea or not)
;; in maps you can use keywords as functions IF they are already part of the maps keys"
(:d m)

;; keywords that are NOT in the map already won't work
(:z m) ;; :z is undefined

;; BUT once a keyword fn has been defined then it can be used (see also below with keywords in sequence fns)
(map :z [])

;; should work after running above code
(:z m)

;; dlet is a destructuring let (hasn't been tested nearly enough (yet) and may need to be re-written at some point; impl is a bit hackish imo))
(dlet (({:keys [A a b] {:keys [f]} :d :as my-map} m)
       ([_ _ third] (get-in m [:d :f])))
  [A a b f third])

;; metadata can be added to any clos object (with-meta, meta, vary-meta))
(defclass foo (metadata)
  ((data :initarg :data :accessor data)))

;; can add a hash-table or any hamt hash-map [persistent or transient] to
;; a CLOS object as metadata when the CLOS object is derived from the
;; metadata class as above

(def obj (with-meta (make-instance 'foo :data "some data") %{:c 3 :d 4})) ;; hash-table as meta

(meta obj)

;; transient data structures (CLOS, hamt, or bpvt) can have transient or persistent meta hash-maps OR a lisp hash-table
;;
;; ex. this is a transient hash map, with a native lisp hash-table as the meta object.
;;     also note that the lisp hash-table contains a persistent vector and a persistent set
(def tm (with-meta @{:a 1 :b 2} %{:d [1 2 3] :e #{'a 'b 'c}}))

(:d (meta tm))

;; persistent data structures are restricted to persistent hash map meta data
;; wont work-->  (def pm (with-meta {:a 1 :b 2} @{:d 4 :e 5}))

;; will work
(def pm (with-meta {:a 1 :b 2} {:d 4 :e 5}))
(def ps (with-meta #{:a :b :c} {:keywords 3}))
(meta pm)
(meta ps)

(def my-vec (map #'persistent-hash-map  (repeat :a) (range 100)))

;; in sequence functions you can use keywords as functions
(map :a my-vec)

;; even if the map doesn't have that keyword as a key
(map :b my-vec)

;; more examples and docs to come (hopefully)
