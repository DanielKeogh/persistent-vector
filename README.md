# persistent-vector

A fast Persistent/Immutable Vector implementation based upon Clojure's. This vector is also known as a 'Vector Trie'.

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
