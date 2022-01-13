# persistent-vector

A fast Persistent/Immutable Vector implementation based upon Clojure's. This vector is also known as a 'Vector Trie'.

By default, persistent-vector is immutable, but this implementation also supports transients for building vectors significantly faster.

This vector has the following Big-O complexity:

| Action | Asymptotic Complexity |
| -------- | ----------------------- |
| Random Access | O(log32n) |
| Replace | O(log32n) |
| Append/Push | O(1) |
| Pop | O(1) |
| Count | O(1) |

## Usage

For convenience of using this library without conflicting with other packages, functions that transform or read from vectors are prefixed with `v-`.

**Constructor:**

```lisp
(pv:vec 0 1 2 3)
;; [0 1 2 3]
```

**Constructor with transience:**

Use `with-transient` to build vectors quickly.

All methods that work on a persistent vector will work on a transient vector by mutating it in place, but unlike the persistent vector, transient vectors are not thread safe.

```lisp
(pv:with-transient (trans (pv:vec))
	(dotimes (x 4)
		 (pv:pv-append trans x)))
;; [0 1 2 3]
```

**Set index:**

```lisp
(pv:v-set-at (pv:vec 1 2 3 4) 1 "foo")
;; [0 "foo" 3 4]
```
**Append:**

```lisp
(pv:v-append (pv:vec 1) 2)
;; [1 2]
```

**Pop Last:**

```lisp
(pv:v-pop-last (pv:vec 1 2 3))
;; [1 2]
```

**Equality:**

By default, equality of elements uses `equal`:

```lisp
(pv:v-equal (pv:vec 1 2 3) (pv:vec 1 2 3))
;; true

(pv:v-equal (pv:vec 1 2 3) (pv:vec))
;; false
```

It can be overriden via an optional argument:

```lisp
(pv:v-equal (pv:vec '(1 2) 2 3) (pv:vec '(1 2) 2 3) #'eq)
;; false

(pv:v-equal (pv:vec '(1 2) 2 3) (pv:vec '(1 2) 2 3) #'equal)
;; true 
```

**Looping:**

There are three functions provided for looping over vectors.

`(v-map vector (lambda (x)))` is for building new collections. It returns a linked list.

```lisp
(pv:v-map (pv:vec 1 2 3) (lambda (x) (* 2 x)))
;; (2 4 6)
```

`(v-for vector (lambda (x)))` is for loop over all elements in a vector.

```lisp
(pv:v-for (pv:vec 1 2 3) (lambda (x) (* 2 x)))
;; nil
```

`(v-reduce vector (lambda (aggregate val)) &optional starting-agggregate)` is for aggregating values in a vector.

```lisp
(pv:v-reduce 
	(pv:vec 1 2 3) 
	(lambda (aggregate val) 
		(+ aggregate val)) 
	0)
;; 6
```

## Thread Safety

In theory the Persistent Vector is completely thread safe. This has been tested casually but never in a production system. If you ever end up using this in production code please let me know. :)

## Supported Lisps

This should work in all Common Lisp implementations.

It has been test in SBCL and CLisp.

## Benchmarking

Appending 1000000 items to a vector in 150ms:

```lisp
(time (loop for i from 0 to 1000000
		     for vec = (pv:vec) then (pv:v-append vec i)))
;Evaluation took:
;  0.146 seconds of real time
;  0.146716 seconds of total run time (0.133379 user, 0.013337 system)
;  [ Run times consist of 0.043 seconds GC time, and 0.104 seconds non-GC time. ]
;  100.68% CPU
;  526,666,278 processor cycles
;  229,177,984 bytes consed

```

Using `with-transient` to build the vector is an order of magnitude faster:

```lisp
(time (pv:with-transient (vec (pv:vec))
		 (dotimes (i 1000000)
		   (pv:v-append vec i))))
;Evaluation took:
;  0.019 seconds of real time
;  0.022920 seconds of total run time (0.022798 user, 0.000122 system)
;  121.05% CPU
;  82,677,537 processor cycles
;  9,810,304 bytes consed
```
