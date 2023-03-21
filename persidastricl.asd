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
  :license  "Use at your own risk; NO promises, NO obligations, NO REALLY, I do not care! Seriously!"
  :version "0.0.3"

  :depends-on (#:arrow-macros
               #:babel
               #:cl-ppcre
               #:cl-murmurhash
               #:closer-mop
               #:json-streams
               #:named-readtables
               #:sip-hash
               #:stmx)
  :serial t

  :components ((:module "packaging"
                :pathname "src"
                :serial t
                :components ((:file "package")))

               (:module "init"
                :pathname "src/init"
                :serial t
                :components ((:file "bits")
                             (:file "hash")
                             (:file "vector")
                             (:file "equality")
                             (:file "macros")
                             (:file "functions")
                             (:file "methods")))

               (:module "stm"
                :pathname "src/stm"
                :serial t
                :components ((:file "atom")))

               (:module "lazy-sequences"
                :pathname "src/lazy"
                :serial t
                :components ((:file "thunk")
                             (:file "lazy-sequence")))

               (:module "metaclass"
                :pathname "src/metaclass"
                :serial t
                :components ((:file "immutable-class")))

               (:module "src/bitmap-vector"
                :serial t
                :components ((:file "bitmap-vector")
                             (:file "key-value-bitmap-vector")
                             (:file "node-bitmap-vector")
                             (:file "transient-bitmap-vector")
                             (:file "persistent-bitmap-vector")
                             (:file "transient-key-value-bitmap-vector")
                             (:file "persistent-key-value-bitmap-vector")
                             (:file "transient-node-bitmap-vector")
                             (:file "persistent-node-bitmap-vector")))

               (:module "node"
                :pathname "src/node"
                :serial t
                :components ((:file "node")

                             (:file "overflow-node")
                             (:file "persistent-overflow-node")
                             (:file "transient-overflow-node")
                             (:file "hash-set-overflow-node")
                             (:file "hash-map-overflow-node")
                             (:file "transient-hash-set-overflow-node")
                             (:file "persistent-hash-set-overflow-node")
                             (:file "transient-hash-map-overflow-node")
                             (:file "persistent-hash-map-overflow-node")

                             (:file "hamt-node")
                             (:file "transient-node")
                             (:file "persistent-node")
                             (:file "hash-map-node")
                             (:file "hash-set-node")
                             (:file "transient-hash-map-node")
                             (:file "persistent-hash-map-node")
                             (:file "transient-hash-set-node")
                             (:file "persistent-hash-set-node")

                             (:file "bpvt-node")
                             (:file "vector-node")
                             (:file "vector-leaf-node")
                             (:file "transient-vector-node")
                             (:file "persistent-vector-node")
                             (:file "transient-vector-leaf-node")
                             (:file "persistent-vector-leaf-node")))

               (:module "mixins"
                :pathname "src/mixins"
                :serial t
                :components ((:file "associable")
                             (:file "collection")
                             (:file "counted")
                             (:file "meta")
                             (:file "seqable")))

               (:module "structures"
                :pathname "src/impl"
                :serial t
                :components ((:file "bpvt")

                             (:module "vector"
                              :pathname "vector"
                              :serial t
                              :components ((:file "vector")
                                           (:file "transient-vector")
                                           (:file "persistent-vector")))

                             (:file "hamt")

                             (:module "set"
                              :pathname "set"
                              :serial t
                              :components ((:file "hash-set")
                                           (:file "transient-hash-set")
                                           (:file "persistent-hash-set")))

                             (:module "map"
                              :pathname "map"
                              :serial t
                              :components ((:file "entry")
                                           (:file "hash-map")
                                           (:file "transient-hash-map")
                                           (:file "persistent-hash-map")))

                             (:file "functions")
                             (:file "methods")
                             (:file "syntax")))

               (:module "iterator"
                :pathname "src/iterator"
                :serial t
                :components ((:file "iterator")
                             (:file "node-iterator")
                             (:file "hamt-iterator")
                             (:file "methods")))

               (:module "core"
                :pathname "src/core"
                :serial t
                :components ((:file "methods")
                             (:file "functions")
                             (:file "destructure")
                             (:file "when-let")
                             (:file "string")
                             (:file "set")
                             (:file "walk")
                             (:file "json")))

               (:module "sample-seqs"
                :pathname "src/lazy"
                :serial t
                :components ((:file "sequences")))

               (:module "user"
                :pathname "src/user"
                :serial t
                :components ((:file "user"))))

  :in-order-to ((test-op (test-op #:persidastricl/test))))


(asdf:defsystem #:persidastricl/test
  :depends-on (#:persidastricl
               #:fiveam)

  :components ((:module "test"
                :serial t
                :components ((:file "master")
                             (:file "immutable-class")
                             (:file "bits")
                             (:file "hash")
                             (:file "entry")
                             (:file "vector")
                             (:file "bitmap-vector")
                             (:file "hash-map")
                             (:file "hash-set"))))

  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :master-suite :persidastricl))))
