;;;; tests/main.lisp

(in-package #:persistent-vector-tests)

(def-suite all-tests
  :description "Main test suite for persistent-vector")

(in-suite all-tests)

(defun test-persistent-vector ()
  (run! 'all-tests))

(test vec
  (is (pv:vector-trie-p (pv:vec)))
  (is (pv:vector-trie-p (pv:vec 1 2 3)))
  (is (pv:vector-trie-p (pv:vec nil nil nil)))
  (is (pv:vector-trie-p (pv:vec "banana" 0.2 (/ 1 2))))
  (is (pv:vector-trie-p (pv:vec 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (is (pv:vector-trie-p (apply #'pv:vec (loop for i from 1 to 50 collect i)))))

(test val-at
  (let ((v (pv:vec 1 2 3)))
    (is (= 1 (pv:pv-val-at 0)))
    (is (= 2 (pv:pv-val-at 1)))
    (is (= 3 (pv:pv-val-at 2)))))

(test pv-equal
  (is (pv:pv-equal (pv:vec 1 2 3)
		   (pv:vec 1 2 3)))
  (is (pv:pv-equal (pv:vec "1" 2 nil)
		   (pv:vec "1" 2 nil)))
  (is (pv:pv-equal (pv:vec)
		   (pv:vec)))
  (is (not (pv:pv-equal (pv:vec)
			(pv:vec 1))))
  (is (not (pv:pv-equal (pv:vec 1)
			(pv:vec))))
  (is (not (pv:pv-equal (pv:vec 1 2 3)
			(pv:vec 3 2 1)))))

(test pv-append
  (is (pv:pv-equal (pv:vec 1 2 3) (pv:pv-append (pv:vec 1 2) 3)))
  (is (= 3 (pv:pv-val-at (pv:pv-append (pv:vec 1 2) 3) 2)))
  (is (= 3 (pv:pv-length (pv:pv-append (pv:vec 1 2) 3)))))

(test pv-append-many
  (let ((v (loop for i from 0 below 10000
		 for v = (pv:vec i) then (pv:pv-append v i)
		 finally (return v))))
    (is (= 10000 (pv:pv-length v)))
    (loop for i from 0 below 10000
	  do (is (= i (pv:pv-val-at v i))))))
