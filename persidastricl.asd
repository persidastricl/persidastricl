;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

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
  :license  "Eclipse Public License 2.0"
  :version "0.3.0"

  :depends-on (#:arrow-macros
               #:babel
               #:cl-ppcre
               #:cl-murmurhash
               #:closer-mop
               #:json-streams
               #:named-readtables
               #:sip-hash
               #:random-state
               #:atomics)
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
                             (:file "methods")
                             (:file "compare")
                             (:file "entry")))

               (:module "lazy-sequences"
                :pathname "src/lazy"
                :serial t
                :components ((:file "thunk")
                             (:file "lazy-sequence")))

               (:module "metaclass"
                :pathname "src/metaclass"
                :serial t
                :components ((:file "immutable-class")))

               (:module "node"
                :pathname "src/node"
                :serial t
                :components ((:file "node")

                             (:file "overflow-node")
                             (:file "hash-set-overflow-node")
                             (:file "hash-map-overflow-node")
                             (:file "transient-hash-set-overflow-node")
                             (:file "persistent-hash-set-overflow-node")
                             (:file "transient-hash-map-overflow-node")
                             (:file "persistent-hash-map-overflow-node")

                             (:file "hamt-node")
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
                :components ((:file "hamt")

                             (:module "map"
                              :pathname "map"
                              :serial t
                              :components ((:file "hash-map")
                                           (:file "transient-hash-map")
                                           (:file "persistent-hash-map")))

                             (:module "set"
                              :pathname "set"
                              :serial t
                              :components ((:file "hash-set")
                                           (:file "transient-hash-set")
                                           (:file "persistent-hash-set")))

                             (:file "bpvt")

                             (:module "vector"
                              :pathname "vector"
                              :serial t
                              :components ((:file "vector")
                                           (:file "transient-vector")
                                           (:file "persistent-vector")))

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

               (:module "stm"
                :pathname "src/stm"
                :serial t
                :components ((:file "atom")))

               (:module "core"
                :pathname "src/core"
                :serial t
                :components ((:file "methods")
                             (:file "functions")
                             (:file "sub-vector")
                             (:file "destructure")
                             (:file "when-first")
                             (:file "string")
                             (:file "set")
                             (:file "walk")
                             (:file "data")
                             (:file "json")
                             (:file "combinatorics")))

               (:module "sample-seqs"
                :pathname "src/lazy"
                :serial t
                :components ((:file "sequences")))

               (:module "user"
                :pathname "src/user"
                :serial t
                :components ((:file "user")
                             (:file "random"))))

  :in-order-to ((test-op (test-op #:persidastricl/test))))


(asdf:defsystem #:persidastricl/test
  :depends-on (#:fiveam
               #:persidastricl)

  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "master")
                             (:file "immutable-class")
                             (:file "bits")
                             (:file "hash")
                             (:file "entry")
                             (:file "vector")
                             (:file "atom")
                             (:file "thunk")
                             (:file "lazy-sequence")
                             (:file "syntax")
                             (:file "hash-map")
                             (:file "hash-set")
                             (:file "equality")
                             (:file "string")
                             (:file "set")
                             (:file "walk")
                             (:file "data")
                             (:file "combinatorics")
                             (:file "random"))))

  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :master-suite :persidastricl/test))))
