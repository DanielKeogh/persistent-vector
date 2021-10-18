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

Constructor:

```lisp
(pv:vec 0 1 2 3)
;; [0 1 2 3]
```

Constructor with transience:

```lisp
(pv:with-transient (trans (pv:vec))
	(dotimes (x 4)
		 (pv:pv-append trans x)))
;; [0 1 2 3]
```

Append:
```lisp
(pv:pv-append (pv:vec 1) 2)
;; [1 2]
```

Pop Last:
```lisp
(pv:pv-poplast (pv:vec 1 2 3))
;; values 3, [1 2]
```