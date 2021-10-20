;;;; api.lisp

(in-package :persistent-vector)

(defun vec (&rest items)
  (apply #'create-persistent-vector items))

(defun pv-val-at (vec n)
  "Get the value at the index n of a persistent vector. If the index is outside of the persistent vector's boundaries, an error will be raised"
  (pv-nth vec n))

(defun pv-set-at (vec n val)
  "Set the index of a persistent vector to a specific value. Setting the index of the (pv-length vector) will append, otherwise, setting a value outside of the persistent vector's boundaries will cause error to be raised"
  (pv-assoc-n vec n val))

(defun pv-append (vec val)
  "Append a value to a persistent vector"
  (pv-cons vec val))

(defun pv-poplast (vec)
  (error "not implemented"))

(defun pv-length (vec)
  "The number of elements in a persistent vector"
  (pv-count vec))

(defun pv-equal (v1 v2 &optional (comparer #'equal))
  "Check if each element of two persistent vectors are equal."
  (and (= (pv-length v1) (pv-length v2))
       (loop with itr1 = (pv-make-iterator v1)
	     with itr2 = (pv-make-iterator v2)
	     for (remaining1 val1) = (multiple-value-list (funcall itr1))
	     for (remaining2 val2) = (multiple-value-list (funcall itr2))
	     while (and remaining1 remaining2)
	     always (funcall comparer val1 val2))))

(defun pv-map (vec fn)
  "Apply (lambda (val)) to element of a persistent vector and collect the results into a list"
  (loop with itr1 = (pv-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr1))
	while remaining
	collect (funcall fn val)))

(defun pv-reduce (vec fn &optional start-val)
  "Apply (lambda (aggregate val)) to aggregate all elements of a persistent vector"
  (loop with itr1 = (pv-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr1))
	while remaining
	for result = (funcall fn start-val val) then (funcall fn result val)
	finally (return result)))

(defmacro with-transient ((var vector) &body body)
  

  )
