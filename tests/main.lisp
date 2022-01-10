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
    (is (= 1 (pv:v-val-at v 0)))
    (is (= 2 (pv:v-val-at v 1)))
    (is (= 3 (pv:v-val-at v 2)))))

(test v-equal
  (is (pv:v-equal (pv:vec 1 2 3)
		   (pv:vec 1 2 3)))
  (is (pv:v-equal (pv:vec "1" 2 nil)
		   (pv:vec "1" 2 nil)))
  (is (pv:v-equal (pv:vec)
		   (pv:vec)))
  (is (not (pv:v-equal (pv:vec)
			(pv:vec 1))))
  (is (not (pv:v-equal (pv:vec 1)
			(pv:vec))))
  (is (not (pv:v-equal (pv:vec 1 2 3)
			(pv:vec 3 2 1)))))

(test v-append
  (is (pv:v-equal (pv:vec 1 2 3) (pv:v-append (pv:vec 1 2) 3)))
  (is (= 3 (pv:v-val-at (pv:v-append (pv:vec 1 2) 3) 2)))
  (is (= 3 (pv:v-length (pv:v-append (pv:vec 1 2) 3)))))

(test v-append-many
  (let ((v (loop for i from 0 below 10000
		 for v = (pv:vec i) then (pv:v-append v i)
		 finally (return v))))
    (is (= 10000 (pv:v-length v)))
    (is (loop for i from 0 below 10000
	      always (= i (pv:v-val-at v i))))))

(test v-equal
  (is (pv:v-equal (pv:vec 1 2 3) (pv:vec 1 2 3)))
  (is (pv:v-equal (pv:vec) (pv:vec)))
  (is (pv:v-equal (pv:vec "Japan" 1 nil) (pv:vec "Japan" 1 nil)))
  (is (not (pv:v-equal (pv:vec) (pv:vec "Japan" 1 nil))))
  (is (not (pv:v-equal (pv:vec) (pv:vec nil))))
  (is (not (pv:v-equal (pv:vec nil) (pv:vec))))
  (is (not (pv:v-equal (pv:vec 3 2 1) (pv:vec 1 2 3)))))

(test with-transient
  (is (pv:v-equal (pv:vec) (pv:with-transient (v (pv:vec)))))
  (is (pv:v-equal (pv:vec 1 2 3) (pv:with-transient (v (pv:vec 1 2 3)))))
  (pv:with-transient (v (pv:vec))
    (is (pv:v-equal (pv:vec) v)))

  (pv:with-transient (v (pv:vec 1 2 3))
    (is (pv:v-equal (pv:vec 1 2 3) v))))
