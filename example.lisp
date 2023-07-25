;; -----
;;
;; -*- mode: Lisp; -*-
;;
;;  example.lisp
;;

;; load up the persidastricl library
(ql:quickload :persidastricl)

;; switch to the case-sensitive/syntactic sugar
(named-readtables:in-readtable persidastricl:syntax)

;;
;; now define the package where my code will go and use persidastricl there
;; NOTE: you have to shadow those things that are re-written from CL
;;
(defpackage #:example
  (:use #:cl #:persidastricl)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:get
                          #:keyword
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:nth
                          #:pop
                          #:reduce
                          #:remove
                          #:replace
                          #:rest
                          #:second
                          #:set
                          #:some
                          #:third
                          #:vector))

;; get into the package to write our code
(in-package #:example)


;; turn on the syntax here too
(named-readtables:in-readtable persidastricl:syntax)

;; now write some code (all this below is very contrived and just pointless besides for demo!!)

(def m {:A 0 :a 1 :b 2 :c 3 :d {:e #{1 2 3 4 5} :f [1 2 3 4 5] :g '(9 8 7 6 5)}})

;; BEWARE: notice that in the persidastricl world things are case-sensitive

(defun doit ()
  (dlet (({:keys [A a b] {:keys [f]} :d :as my-map} m))
    [A a b f (:g (:d (select-keys my-map [:d])))]))

(doit)
