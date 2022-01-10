;;;; api.lisp

(in-package :persistent-vector)

(defun vec (&rest items)
  (apply #'create-persistent-vector items))

(defun v-val-at (vec n)
  "Get the value at the index n of a persistent vector. If the index is outside of the persistent vector's boundaries, an error will be raised"
  (vec-val-at vec n))

(defun v-set-at (vec n val)
  "Set the index of a persistent vector to a specific value. Setting the index of the (pv-length vector) will append, otherwise, setting a value outside of the persistent vector's boundaries will cause error to be raised"
  (vec-assoc-n vec n val))

(defun v-append (vec val)
  "Append a value to a persistent vector"
  (vec-cons vec val))

(defun v-poplast (vec)
  (error "not implemented"))

(defun v-length (vec)
  "The number of elements in a persistent vector"
  (vec-count vec))

(defun v-equal (v1 v2 &optional (comparer #'equal))
  "Check if each element of two persistent vectors are equal."
  (and (= (pv-length v1) (pv-length v2))
       (loop with itr1 = (pvec-make-iterator v1)
	     with itr2 = (pvec-make-iterator v2)
	     for (remaining1 val1) = (multiple-value-list (funcall itr1))
	     for (remaining2 val2) = (multiple-value-list (funcall itr2))
	     while (and remaining1 remaining2)
	     always (funcall comparer val1 val2))))

(defun v-map (vec fn)
  "Apply (lambda (val)) to element of a persistent vector and collect the results into a list"
  (loop with itr1 = (vec-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr1))
	while remaining
	collect (funcall fn val)))

(defun v-reduce (vec fn &optional start-val)
  "Apply (lambda (aggregate val)) to aggregate all elements of a persistent vector"
  (loop with itr1 = (vec-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr1))
	while remaining
	for result = (funcall fn start-val val) then (funcall fn result val)
	finally (return result)))

(defmacro with-transient ((var vector) &body body)
  (let ((vecsym (gensym)))
    `(let* ((,vecsym (pv-as-transient ,vector))
	    (,var ,vecsym))
       (progn ,@body)
       (tv-as-persistent ,vecsym))))
