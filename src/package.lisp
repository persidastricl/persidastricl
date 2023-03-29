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

(defpackage #:bits
  (:nicknames #:bit #:b)
  (:use #:cl)
  (:shadow #:cl #:set)
  (:export #:set?
           #:clear
           #:set
           #:bits
           #:below
           #:index))

(defpackage #:hash
  (:nicknames #:h)
  (:use #:cl #:cl-murmurhash)
  (:export #:hash
           #:size))

(defpackage #:vector
  (:nicknames #:v)
  (:use #:cl)
  (:shadow #:cl #:append #:delete)
  (:export #:copy
           #:append
           #:insert
           #:update
           #:delete
           #:modify))

(defpackage #:persidastricl
  (:nicknames :pds :p)
  (:use #:cl #:arrow-macros)
  (:shadow #:cl
           #:some
           #:atom
           #:keyword
           #:cons
           #:first
           #:second
           #:third
           #:rest
           #:nth
           #:last
           #:butlast
           #:assoc
           #:dissoc
           #:get
           #:delete
           #:remove
           #:length
           #:count
           #:set
           #:vector
           #:merge
           #:pop
           #:reduce
           #:replace
           #:map)
  (:export
   #:-<>
   #:-<>>
   #:->
   #:->>
   #:->alist
   #:->array
   #:->list
   #:->plist
   #:->vector
   #:-vec
   #:<!>
   #:<>
   #:==
   #:as->
   #:assoc
   #:assoc-in
   #:atom
   #:bounded-count
   #:butlast
   #:collection?
   #:comment
   #:comp
   #:concat
   #:cond->
   #:cond->>
   #:conj
   #:cons
   #:contains?
   #:count
   #:cycle
   #:dec
   #:dedup
   #:def
   #:defmemoized
   #:delay
   #:deref
   #:destructure
   #:disj
   #:dissoc
   #:distinct
   #:distinct?
   #:dlet
   #:do-n
   #:doall
   #:dorun
   #:dorun-n
   #:dotted-pair?
   #:drop
   #:drop-last
   #:drop-while
   #:empty
   #:empty?
   #:even?
   #:every-pred
   #:every?
   #:fact
   #:false?
   #:fdef
   #:filter
   #:filterv
   #:first
   #:flatten
   #:fn
   #:fnil
   #:force
   #:frequencies
   #:get
   #:get-in
   #:group-by
   #:identical?
   #:if-let
   #:if-not
   #:inc
   #:instance?
   #:int?
   #:integers
   #:interleave
   #:interpose
   #:into
   #:iterate
   #:juxt
   #:keep
   #:keep-indexed
   #:key
   #:keys
   #:keyword
   #:last
   #:lazy-cat
   #:lazy-seq
   #:length
   #:line-seq
   #:lookup
   #:lseq
   #:map
   #:map-indexed
   #:map?
   #:mapcat
   #:mapv
   #:max-key
   #:memoize
   #:merge
   #:merge-with
   #:meta
   #:min-key
   #:n-choose-k
   #:name
   #:nat-int?
   #:neg-int?
   #:neg?
   #:next
   #:nil?
   #:not-any?
   #:not-every?
   #:nth
   #:odd?
   #:only-valid-values
   #:partial
   #:partition
   #:partition-all
   #:partition-by
   #:peek
   #:persistent-hash-map
   #:persistent-hash-set
   #:persistent-vector
   #:pop
   #:pos-int?
   #:pos?
   #:put
   #:quot
   #:range
   #:re-seq
   #:reduce
   #:reduce-kv
   #:reductions
   #:repeat
   #:repeatedly
   #:replace
   #:reset!
   #:rest
   #:run!
   #:second
   #:select-keys
   #:seq
   #:sequential?
   #:set
   #:set?
   #:shuffle
   #:slurp
   #:some
   #:some-<>
   #:some-<>>
   #:some->
   #:some->>
   #:some-fn
   #:some?
   #:spit
   #:split-at
   #:split-with
   #:str
   #:string?
   #:subs
   #:subseq
   #:swap!
   #:syntax
   #:t-set
   #:t-vec
   #:take
   #:take-last
   #:take-nth
   #:take-while
   #:third
   #:trampoline
   #:transient!
   #:transient-hash-map
   #:transient-hash-set
   #:transient-vector
   #:tree-seq
   #:true?
   #:update
   #:update-in
   #:val
   #:vals
   #:value
   #:vary-meta
   #:vec
   #:vector?
   #:when-first
   #:when-let
   #:when-not
   #:while
   #:with-meta
   #:zero?
   #:zipmap))


(defpackage #:string
  (:nicknames #:s #:str)
  (:use #:cl #:persidastricl)
  (:shadow #:cl #:replace #:reverse)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:keyword
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:second
                          #:third
                          #:nth
                          #:get
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:pop
                          #:reduce
                          #:remove
                          #:rest
                          #:set
                          #:some
                          #:vector)
  (:export #:blank?
           #:capitalize
           #:condense
           #:ends-with?
           #:escape
           #:includes?
           #:index-of
           #:join
           #:last-index-of
           #:lower-case
           #:re-quote-replacement
           #:replace
           #:replace-first
           #:reverse
           #:split
           #:split-lines
           #:starts-with?
           #:trim
           #:trim-newline
           #:triml
           #:trimr
           #:upper-case))

(defpackage #:set
  (:use #:cl #:persidastricl)
  (:shadow #:cl #:union #:intersection)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:keyword
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:second
                          #:third
                          #:nth
                          #:get
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:pop
                          #:reduce
                          #:remove
                          #:replace
                          #:rest
                          #:set
                          #:some
                          #:vector)
  (:export #:difference
           #:index
           #:intersection
           #:join
           #:map-invert
           #:project
           #:rename
           #:rename-keys
           #:select
           #:subset?
           #:superset?
           #:union))

(defpackage #:walk
  (:use #:cl #:persidastricl)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:keyword
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:second
                          #:third
                          #:nth
                          #:get
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:pop
                          #:reduce
                          #:remove
                          #:replace
                          #:rest
                          #:set
                          #:some
                          #:vector)
  (:export
   #:keywordize-keys
   #:macroexpand-all
   #:postwalk
   #:postwalk-demo
   #:postwalk-replace
   #:prewalk
   #:prewalk-demo
   #:prewalk-replace
   #:stringify-keys
   #:walk))


(defpackage #:data
  (:use #:cl #:persidastricl)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:keyword
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:second
                          #:third
                          #:nth
                          #:get
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:pop
                          #:reduce
                          #:remove
                          #:replace
                          #:rest
                          #:set
                          #:some
                          #:vector)
  (:export #:diff))

(defpackage #:json
  (:use #:cl #:persidastricl #:json-streams)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:keyword
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:second
                          #:third
                          #:nth
                          #:get
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:pop
                          #:reduce
                          #:remove
                          #:replace
                          #:rest
                          #:set
                          #:some
                          #:vector)
  (:export #:decode-file #:decode-string))

(defpackage #:user
  (:use #:cl #:persidastricl #:arrow-macros)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
                          #:keyword
                          #:butlast
                          #:cons
                          #:count
                          #:delete
                          #:filter
                          #:first
                          #:second
                          #:third
                          #:nth
                          #:get
                          #:last
                          #:length
                          #:map
                          #:merge
                          #:pop
                          #:reduce
                          #:remove
                          #:replace
                          #:rest
                          #:set
                          #:some
                          #:vector))
