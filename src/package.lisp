;;;; package.lisp

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
           #:map)
  (:export
   #:->alist
   #:->array
   #:->keys
   #:->list
   #:->plist
   #:->vals
   #:->vector
   #:->keyword
   #:-vec
   #:==
   #:assoc
   #:assoc-in
   #:atom
   #:bounded-count
   #:butlast
   #:collection?
   #:comment
   #:concat
   #:conj
   #:cons
   #:contains?
   #:count
   #:cycle
   #:dec
   #:deref
   #:disj
   #:dissoc
   #:drop
   #:drop-last
   #:drop-while
   #:empty
   #:empty?
   #:even?
   #:every?
   #:false?
   #:filter
   #:filterv
   #:first
   #:second
   #:third
   #:nth
   #:flatten
   #:fn
   #:fnil
   #:get
   #:get-in
   #:group-by
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
   #:last
   #:lazy-seq
   #:length
   #:line-seq
   #:map
   #:lookup
   #:reduce
   #:lseq
   #:merge
   #:merge-with
   #:map-indexed
   #:map?
   #:mapcat
   #:mapv
   #:memoize
   #:meta
   #:named-fn
   #:nat-int?
   #:neg-int?
   #:neg?
   #:next
   #:nil?
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
   #:range
   #:reduce-kv
   #:repeat
   #:repeatedly
   #:reset!
   #:rest
   #:run!
   #:seq
   #:sequential?
   #:set
   #:set?
   #:select-keys
   #:some
   #:some-fn
   #:some?
   #:split-at
   #:split-with
   #:str
   #:string?
   #:swap!
   #:syntax
   #:t-set
   #:t-vec
   #:take
   #:take-last
   #:take-nth
   #:take-while
   #:transient!
   #:transient-hash-map
   #:transient-hash-set
   #:transient-vector
   #:tree-seq
   #:true?
   #:update
   #:update-in
   #:vec
   #:vector?
   #:with-meta
   #:zero?
   #:zipmap
   ))


(defpackage #:string
  (:nicknames #:s #:str)
  (:use #:cl #:persidastricl)
  (:shadow #:cl #:replace #:reverse)
  (:shadowing-import-from #:persidastricl
                          #:assoc
                          #:atom
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
           #:union
           ))

(defpackage #:walk
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
   #:walk
   ))
