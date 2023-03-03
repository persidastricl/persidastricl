# persidastricl

## Persistant Data Structures in Common Lisp

This project started out as another implementation of persistent and transient HAMT maps, sets, bags, and vectors in common lisp. After things settled a bit, other things were added a little at a time. These things include lazy sequences, atoms (using stmx), and some common lisp interop around some core functions similar to those found in clojure. The goal was to create a familiar and practical environment where I could work as easily and efficiently in common lisp as I was used to working in clojure.  For my personal uses, the goal was achieved. For anyone else, this might not be the case so use at your own risk.

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

### Bags

TODO

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
```

Maps
```common-lisp
(defvar m1 {:a 1 :b 2 :c 3}
(get m1 :a) ;; => 1
```

Sets
```common-lisp
(defvar s1 #{:a :b :c})
(contains? m1 :a) ;; => T
```

Bags
```
;; todo
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
;; => ([:G 6] [:A 1] [:C 3] ...)

(seq [1 2 3 4 5 6 7 8 9 0])
;; => (1 2 3 ...)

(seq #{1 2 3 4 5 6 7 8 9 0})
;; => (7 1 6 ...)
```

More on lazy sequences below in the Examples section.

## Atoms

At the moment, a very simple (on my part) implementation of atoms exists based on the excellent work in the stmx library for common lisp. There is a `swap!` and a `reset!` function currently.

## Conveniences

Once these three elements were in place (persistent data structures, lazy sequences, and something like atoms), much of the familiar functionality and conveniences that I wanted could easily be implemented.

Much of what you can do with clojure around maps, sets, vectors, and lazy sequences you can do with this library (examples:  map, reduce, reduce-kv, keep, filter, remove, group-by, partition, interpose, juxt ... etc).  See the core functions and methods for more.  There are other functions and methods in various locations within the code that seemed better defined in those particular locations to me.  The list of exported functions in `package.lisp` might also help.

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

(->list [1 2 3 4])
(->plist {:a 1 :b 2}) ;; lazy
(->alist {:a 1 :b 2})
(->array {:a 1 :b 2})

;; TODO: more examples
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
```

This allows some familiar use of maps as functions within the context of the macro for concenience only.

### TODO go over the core functions showing any differences when applicable between these and clojure's

## Examples

### TODO lots more examples

## License

TBD

For now, it is

Copyright (c) 2022-2023 Michael D Pendergrass
Copyright (c) 2022-2023 pupcus.org

until something is possibly useful enough to release as open source.

Even then, this library, if EVER provided, is provided as-is; use at your own risk; no warranties; no guarantees; and NO obligations on my part at all!

No, listen! Seriously, I could not care less if it ever works for you or not. I wrote this because I wanted it

Having said that, I would love it if people help make it better. If you can make it better, then you don't need ME to make it better and we all win. I will continue to do my best to make it faster, more complete, and more useful. All I am saying here is that I make no promises I will do things how you want, when you want, or even if IF you want.  If it does not work or work well on 'some other lisp', I may or may not care.[A

I will likely release this under a BSD style license at some point soon

