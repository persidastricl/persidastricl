;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; persidastricl.asd
;;;
;;;  persistent data structures in common lisp
;;;
;;; -----

(asdf:defsystem
    #:persidastricl
  :description "persistent data structures in common lisp 'per-si-DAS-trick-el"
  :author "Michael D. Pendergrass <mdp@pupcus.org>"
  :license  "use at your own risk; no promises, no obligations, I do not care. Seriously."
  :version "0.0.1"
  :serial t

  :depends-on (#:arrow-macros
               #:cl-murmurhash)

  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "persidastricl")
                             (:file "bitop")
                             (:file "entry")
                             (:file "hash")
                             (:file "vector")
                             (:file "util")
                             (:file "counted")
                             (:file "meta")
                             (:file "seqable")
                             (:file "bitmap-vector")
                             (:file "node"))))

  :in-order-to ((test-op (test-op #:persidastricl/test))))


(asdf:defsystem
    #:persidastricl/test
  :depends-on (#:persidastricl
               #:fiveam)

  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "persidastricl")
                             (:file "bitop")
                             (:file "entry")
                             (:file "hash")
                             (:file "vector")
                             (:file "bitmap-vector")
                             (:file "node"))))

  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :master-suite :persidastricl-test))))
