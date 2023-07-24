# persidastricl

## Persistant Data Structures in Common Lisp

This project is very early ALPHA code. Everything! is subject to change so please use/proceed with caution.

This only works with `sbcl` at the moment.

This project started out as a learning experience and another implementation of persistent and transient HAMT maps, sets, and vectors in common lisp. After things settled a bit, other things were added a little at a time. These things include lazy sequences, basic atoms, and some common lisp interop around some core functions similar to those found in clojure. The goal was to create a familiar and practical environment where I could work as easily and efficiently in common lisp as I was used to working in clojure.  For my personal uses, the goal was achieved. For anyone else, this might not be the case so use at your own risk.

## Building and Testing

```common-lisp
(ql:quickload :fiveam)
(ql:quickload :persidastricl)
(in-package :persidastricl)
(named-readtables:in-readtable persidastricl:syntax)
(ql:quickload :persidastricl/test)
(asdf:test-system :persidastricl) 
```

## Data Structures

### Vectors

Vectors contain values in order by an index

```common-lisp
(defvar v1 (persistent-vector 1 2 3 4 5))
(get v1 0) ;;=> 1
```

### Maps

You add keys and values and then pull them back out when you need them.

```common-lisp
(defvar m1 (peristent-hash-map :a 1 :b 2))
(get m1 :a) ;; => 1
```

### Sets

Sets contain one and only one copy of any values you put in. You can check for set membership with `contains?`

```common-lisp
(defvar s1 (persistent-hash-set :a :b :c))
(contains? s1 :a) ;; => T
```

## Syntax

When convenient, there exists a dash of syntactic sugar for these data structures (and others to be shown below). I used named-readtables for this.  To turn-on the syntax use:

```common-lisp
(named-readtables:in-readtable persidastricl:syntax)
```

When using the reader-macros defined you can do things like the following:

Vectors
```common-lisp
(defvar v1 [1 2 3 4 5])
(get v1 0) ;; => 1

;; transient versions
(defvar v1 @[1 2 3 4 5])
(get v1 0) ;; => 1
```

Maps
```common-lisp
(defvar m1 {:a 1 :b 2 :c 3}
(get m1 :a) ;; => 1

;; transient versions
(defvar m1 @{:a 1 :b 2 :c 3})
(get m1 :a) ;; => 1
```

Sets
```common-lisp
(defvar s1 #{:a :b :c})
(contains? s1 :a) ;; => :a

;; transient versions
(defvar s1 @#{:a :b :c})
(contains? s1 :a) ;; => :a
```

## Lazy Sequences

There are also lazy sequences defined for most of the data structures.  There is a method named `seq` that will take a variety of data structures and turn them into lazy sequences.  Lazy sequences have a `print-object` method defined for them that relies on a variable `*print-lazy-items*` which by dfault is set to 10 items. So, when you print a lazy sequence, it will only print out 10 of them (to avoid realizing infinite sequences)

We can make lazy sequences out of any common lisp sequence
```common-lisp
(seq '(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0))
;; => (1 2 3 4 5 6 7 8 9 0 ...)

(seq #(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0))
;; => (1 2 3 4 5 6 7 8 9 0 ...)

(seq "testing 1 2 3")
;; => (#\t #\e #\s #\t #\i #\n #\g #\  #\1 #\  ...)
```

For simplicity, `*print-lazy-items*` has been set to 3 for the followng examples.

All of the persistent data structures can also be made into a lazy sequence:

```common-lisp
(seq {:a 1 :b 2 :c 3 :d 4 :e 5 :g 6})
;; => (:g 6 :a 1 :c 3 ...)

(seq [1 2 3 4 5 6 7 8 9 0])
;; => (1 2 3 ...)

(seq #{1 2 3 4 5 6 7 8 9 0})
;; => (7 1 6 ...)
```

More on lazy sequences below in the Examples section.

## Atoms

At the moment, a very simple (on my part) implementation of atoms exists based on sbcl's `sb-ext:compare-and-swap`.

## Conveniences

Once these three elements were in place (persistent data structures, lazy sequences, and a basic implementation of STM atoms), much of the familiar functionality and conveniences that I wanted could easily be implemented.

Much of what you can do with clojure around maps, sets, vectors, and lazy sequences you can do with this library (examples:  map, reduce, reduce-kv, keep, filter, remove, group-by, partition, interpose, juxt ... etc).  See the core functions and methods for more.  There are other functions and methods in various locations within the code that seemed better defined in those particular locations to me but at the cost of them not being as easy to discover (especially since this library is not yet sufficiently doucmented).  The list of exported functions in `package.lisp` should also help.

Some common lisp conveniences

Using common lisp data structures in a similar way to the new persistent data structures
```common-lisp
(defvar ht (make-hash-table))
(assoc ht :a 1 :b 2 :c 3)
(get ht :b)


(defvar lst '(1 2 3 4 5))
(assoc lst 3 :new)
(get lst 3)

(defvar v #(1 2 3 4 5))
(assoc v 3 :new)
(get v 3)

(into '() #{:a :b :c :d})
(into #() [:a :b :c :d])
(into ht {:a 5 :b 7})

;; TODO: more examples
;; NOTE: there is also not some syntactic sugar around common lisp maps

 %{:a 1 :b 2} ;; a common lisp hash-table 
 
 ;; this will define a common lisp hash-table when the syntax is turned on
```

Functions for creating various types of common lisp data structures for ease of interop

```common-lisp
(->list [1 2 3 4])
(->plist {:a 1 :b 2}) ;; lazy
(->alist {:a 1 :b 2})
(->array {:a 1 :b 2})
```

Using a bit of a hack (which I am still trying to decide if I should leave in the code or not), you can use keywords as functions with maps
```common-lisp
(defvar m1 {:a 1 :b 2})
(:a m1) ;; => 1
```

This is done when assoc'ing into the map. A function is created with the keyword as the symbol to look itself up in any map given as a arg to the function (also, a default value is optional). This may be a BAD idea and I may leave it up to the user to do that when needed.  This can be done with one of two functions:  a `make-funcallable-keyword` and `make-funcallable-keywords` which will do the smae thing under programmers control.

There are a couple of macros to allow a context-dependent use of a map or set as a function.

```common-lisp
(let ((m1 {:a 1 :b 2}))
  (with-funcallable-map (m m1)
    (m :a))) ;; => 1

(let ((s1 #{:a :b :c}))
  (with-funcallable-map (m m1)
    (m :a))) ;; => T
```
## Other Examples

#### reduce
   `reduce` has been shadowed to also take any sequential and/or lazy sequence. NOTE: This is NOT a clojure-style reduce, rather it maintains the flavor of common lisp's native reduce adding an optional keyword `:initial-value` for setting the initial argument of the reduce. 

```common-lisp
(reduce #'+ (range 10))
;; => 45

(reduce #'+ (range 10) :initial-value 2)
;; => 47
```

#### map and mapv
    `map` has been re-defined to take any sequential data structure and lazily 'map' over it applying a fn. `mapv` eagerly does the same returning a persistent vector.

```common-lisp
(map #'identity "test")
;; => (#\t #\e #\s #\t)

(map #'char-code "testing 1 2 3 4")
;; => (116 101 115 116 105 110 103 32 49 32 ...)

(mapv #'inc (range 20)
;; => [1 2 3 4 5 6 7 8 9 0 10]
```

TODO: add more examples around what is available in the library

#### string operations

#### set operations

#### data (diff)

#### walk

#### combinatorics

FUTURE: there is a pdf guide in the works but who knows how long that will take to be presentable. 

## License

  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org

  This program and the accompanying materials are made available under the terms of the Eclipse Public License 2.0 which is available at https://www.eclipse.org/legal/epl-2.0/

  SPDX-License-Identifier: EPL-2.0


