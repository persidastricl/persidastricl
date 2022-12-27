;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; persidastricl.asd
;;;
;;;  persistent data structures in common lisp
;;;
;;; -----

(asdf:defsystem #:persidastricl
  :description "persistent data structures in common lisp 'per-si-DAS-trick-el"
  :author "Michael D. Pendergrass <mdp@pupcus.org>"
  :license  "use at your own risk; no promises, no obligations, I do not care. Seriously."
  :version "0.0.1"

  :depends-on (#:arrow-macros
               #:cl-murmurhash)

  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "equality")
                             (:file "bits")
                             (:file "hash")
                             (:file "entry")
                             (:file "vector")
                             (:file "immutable")
                             (:file "counted")
                             (:file "meta")
                             (:file "associable")
                             (:file "collection")
                             (:file "seqable")
                             (:file "methods")
                             (:file "bitmap-vector")
                             (:file "node")
                             (:file "transient-hash-map")
                             (:file "persistent-hash-map")
                             (:file "hash-set")
                             (:file "transient-hash-set")
                             (:file "persistent-hash-set")
                             )))

  :in-order-to ((test-op (test-op #:persidastricl/test))))


(asdf:defsystem #:persidastricl/test
  :depends-on (#:persidastricl
               #:fiveam)

  :components ((:module "test"
                :serial t
                :components ((:file "master")
                             (:file "bits")
                             (:file "hash")
                             (:file "entry")
                             (:file "vector")
                             (:file "immutable")
                             (:file "bitmap-vector")
                             )))

  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :master-suite :persidastricl))))
