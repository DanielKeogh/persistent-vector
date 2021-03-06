;;;; api.lisp

(in-package :persistent-vector)

(proclaim '(optimize (speed 3) (safety 1) (debug 0)))

(defun vec (&rest items)
  "Create a new persistent vector."
  (apply #'create-persistent-vector items))

(let ((not-found (gensym)))
  (defun val-at (vec n)
    "Get the value at the index n of a persistent vector. If the index is outside of the persistent vector's boundaries, an error will be raised."
    (let ((result
	    (typecase vec
	      (persistent-vector (pv-nth-safe vec n not-found))
	      (transient-vector (tv-val-at vec n not-found)))))
      (if (eq not-found result)
	  (error "Out of bounds")
	  result))))

(defun set-at (vec n val)
  "Set the index of a persistent vector to a specific value. Setting the index of the (length vector) will append, otherwise, setting a value outside of the persistent vector's baoundaries will cause error to be raised."
  (typecase vec
    (persistent-vector (pv-assoc-n vec n val))
    (transient-vector (tv-assoc-n vec n val))))

(defun append (vec val)
  "Append a value to a persistent vector."
  (typecase vec
    (persistent-vector (pv-cons vec val))
    (transient-vector (tv-conj vec val))))

(defun pop-last (vec)
  "Return a new vector without the last item."
  (typecase vec
    (persistent-vector (pv-pop vec))
    (transient-vector (tv-pop-last vec))))

(defun length (vec)
  "Return the element count of a persistent vector."
  (vt-count vec))

(defun equal (v1 v2 &optional (comparer #'cl:equal))
  "Check if each element of two persistent vectors are equal."
  (and (= (length v1) (length v2))
       (loop with itr1 = (vec-make-iterator v1)
	     with itr2 = (vec-make-iterator v2)
	     for (remaining1 val1) = (multiple-value-list (funcall itr1))
	     for (remaining2 val2) = (multiple-value-list (funcall itr2))
	     while (and remaining1 remaining2)
	     always (funcall comparer val1 val2))))

(defun map (vec fn)
  "Apply (lambda (val)) to element of a vector and collect the results into a list."
  (loop with itr1 = (vec-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr1))
	while remaining
	collect (funcall fn val)))

(defun reduce (vec fn &optional start-val)
  "Apply (lambda (aggregate val)) to aggregate all elements of a vector."
  (loop with itr1 = (vec-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr1))
	while remaining
	for result = (funcall fn start-val val) then (funcall fn result val)
	finally (return result)))

(defmacro dovector ((var vector) &body body)
  "Loop over each element of a vector without aggregating the result."
  (let ((itr (gensym))
	(remaining (gensym))
	(val (gensym)))
    `(loop with ,itr = (vec-make-iterator ,vector)
	   for (,remaining ,val) = (multiple-value-list (funcall ,itr))
	   while ,remaining
	   do (let ((,var ,val))
		,@body))))

(defmacro with-transient ((var vector) &body body)
  "Create a transient vector from another vector and bind it to ~var. 
Modifications made to the transient vector will mutate the variable.
Returns a persistent vector holding the elements of resulting transient vector."
  (let ((vecsym (gensym)))
    `(let* ((,vecsym (pv-as-transient ,vector))
	    (,var ,vecsym))
       (progn ,@body)
       (tv-as-persistent ,vecsym))))
