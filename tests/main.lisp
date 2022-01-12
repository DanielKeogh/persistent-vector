;;;; tests/main.lisp

(in-package #:persistent-vector-tests)

(def-suite all-tests
  :description "Main test suite for persistent-vector")

(in-suite all-tests)

(defun test-persistent-vector ()
  (run! 'all-tests))

(defun make-range-vec (range &optional (fn 'identity))
  (pv:with-transient (v (pv:vec))
    (dotimes (i range)
      (pv:v-append v (funcall fn i)))))

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

(test set
  (let* ((v1 (pv:vec 1 2 3))
	 (v2 (pv:v-set-at v1 0 25)))
    (is (= 1 (pv:v-val-at v1 0)))
    (is (= 25 (pv:v-val-at v2 0)))))

(test set-many
  (let* ((v1 (make-range-vec 101))
	 (v2 (loop for i from 0 to 100
		   for next-v = v1 then (pv:v-set-at next-v i (* 2 i))
		   finally (return next-v)))
	 (v2-other (make-range-vec 101 (lambda (x) (* 2 x)))))
    (is (pv:v-equal v2 v2-other))))

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

(test v-pop-last
  (let ((v (pv:vec 1 2 3 4)))
    (is (pv:v-equal (pv:vec 1 2 3) (pv:v-pop-last v)))))

(test v-pop-last-many
  (let* ((v1 (make-range-vec 1000))
	 (v2 (loop for start = v1 then (pv:v-pop-last start)
		   repeat 1000
		   finally (return start))))
    (is (= 1000 (pv:v-length v1)))
    (let ((v (pv:v-pop-last v1)))
      (is (= 999 (pv:v-length v)))
      (is (= 998 (pv:v-val-at v 998))))
    (is (= 0 (pv:v-length v2)))))

(test v-equal
  (is (pv:v-equal (pv:vec 1 2 3) (pv:vec 1 2 3)))
  (is (pv:v-equal (pv:vec) (pv:vec)))
  (is (pv:v-equal (pv:vec "Japan" 1 nil) (pv:vec "Japan" 1 nil)))
  (is (not (pv:v-equal (pv:vec) (pv:vec "Japan" 1 nil))))
  (is (not (pv:v-equal (pv:vec) (pv:vec nil))))
  (is (not (pv:v-equal (pv:vec nil) (pv:vec))))
  (is (not (pv:v-equal (pv:vec 3 2 1) (pv:vec 1 2 3)))))

(test v-map
  (let ((v (make-range-vec 1000)))
    (is (equal (pv:v-map v #'identity) (loop for i below 1000 collect i)))))

(test v-reduce
  (let ((v (make-range-vec 1000)))
    (is (equal (pv:v-reduce v #'+ 0) (loop for i below 1000 sum i)))))

(test with-transient
  (is (pv:v-equal (pv:vec) (pv:with-transient (v (pv:vec)))))
  (is (pv:v-equal (pv:vec 1 2 3) (pv:with-transient (v (pv:vec 1 2 3)))))
  (pv:with-transient (v (pv:vec))
    (is (pv:v-equal (pv:vec) v)))
  (pv:with-transient (v (pv:vec 1 2 3))
    (is (pv:v-equal (pv:vec 1 2 3) v))))

(test transient-set-at
  (let* ((assertion-vec (make-range-vec 1000 (lambda (x) (* 2 x))))
	 (r (pv:with-transient (v (make-range-vec 1000))
	      (dotimes (i 1000)
		(pv:v-set-at v i (* 2 i)))

	      (is (pv:v-equal v assertion-vec)))))
    (is (pv:v-equal r assertion-vec))))

(test transient-append
  (let* ((assertion-vec (make-range-vec 1000))
	 
	 (r (pv:with-transient (v (pv:vec))
	      (dotimes (i 1000)
		(pv:v-append v i))
	      (is (pv:v-equal v assertion-vec)))))
    (is (pv:v-equal r assertion-vec))))

(test transient-pop-last
  (let* ((assertion-vec (pv:vec 0 1 2))
	 (r (pv:with-transient (v (make-range-vec 1000))
	      (loop repeat 997 do
		(pv:v-pop-last v))
	      (is (pv:v-equal v assertion-vec))))
	 (is (pv:v-equal r assertion-vec)))))

(test transient-length
  (pv:with-transient (v (make-range-vec 100))
    (is (= 100 (pv:v-length v)))))

(test transient-map
  (pv:with-transient (v (make-range-vec 1000))
    (is (equal (pv:v-map v #'identity) (loop for i below 1000 collect i)))))

(test transient-reduce
  (pv:with-transient (v (make-range-vec 1000))
    (is (equal (pv:v-reduce v #'+ 0) (loop for i below 1000 sum i)))))
