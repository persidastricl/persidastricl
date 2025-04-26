;;; -----     -*- mode: Lisp; -*-
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

;; -----
;;
;; package.lisp
;;
;;
;; -----

(defpackage #:persidastricl/test
  (:use #:cl #:fiveam #:persidastricl)
  (:shadowing-import-from #:fiveam
                          #:run!)
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
