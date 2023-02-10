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
               #:cl-ppcre
               #:cl-murmurhash
               #:named-readtables
               #:sip-hash)
  :serial t

  :components ((:module "packaging"
                :pathname "src"
                :serial t
                :components ((:file "package")))

               (:module "equality"
                :pathname "src/util"
                :serial t
                :components ((:file "equality")))

               (:module "pre"
                :pathname "src/pre"
                :serial t
                :components ((:file "methods")))

               (:module "utilities"
                :pathname "src/util"
                :serial t
                :components ((:file "string")
                             (:file "equality")
                             (:file "util")
                             (:file "bits")
                             (:file "hash")
                             (:file "entry")
                             (:file "vector")))

               (:module "lazy-sequences"
                :pathname "src/lazy-seq"
                :serial t
                :components ((:file "lazy-sequence")))

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
                             (:file "persistent-vector-node")
                             (:file "persistent-vector-leaf-node")
                             (:file "persistent-vector-tail-node")))

               (:module "mixins"
                :pathname "src/mixins"
                :serial t
                :components ((:file "associable")
                             (:file "collection")
                             (:file "counted")
                             (:file "meta")
                             (:file "seqable")))

               (:module "hamt"
                :pathname "src/impl"
                :serial t
                :components ((:file "hamt")))

               (:module "bpvt"
                :pathname "src/impl"
                :serial t
                :components ((:file "bpvt")))

               (:module "set"
                :pathname "src/impl/set"
                :serial t
                :components ((:file "hash-set")
                             (:file "transient-hash-set")
                             (:file "persistent-hash-set")))

               (:module "map"
                :pathname "src/impl/map"
                :serial t
                :components ((:file "hash-map")
                             (:file "transient-hash-map")
                             (:file "persistent-hash-map")))

               (:module "vector"
                :pathname "src/impl/vector"
                :serial t
                :components ((:file "persistent-vector")))

               (:module "methods"
                :pathname "src"
                :serial t
                :components ((:file "impl/methods")
                             (:file "lazy-seq/methods")))

               (:module "syntax"
                :pathname "src/impl"
                :serial t
                :components ((:file "syntax")))

               (:module "iterator"
                :pathname "src/iterator"
                :serial t
                :components ((:file "iterator")
                             (:file "node-iterator")
                             (:file "hamt-iterator"))))

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
                             (:file "transient-hash-map")
                             (:file "persistent-hash-map")
                             (:file "transient-hash-set")
                             (:file "persistent-hash-set"))))

  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :master-suite :persidastricl))))
