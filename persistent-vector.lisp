;;;; persistent-vector.lisp

(in-package #:persistent-vector)

;;; definition utils

(defconstant +chunk-size+ 32)
(defconstant +chunk-mask+ (1- +chunk-size+))
(defconstant +chunk-bit+ (round (log 32 2)))
;; +chunk-size+ must be a power of 2 or else #'ash tricks dont work
(assert (= +chunk-size+ (expt 2 +chunk-bit+))) 

(defun make-array-chunk ()
  (declare (optimize (speed 3) (safety 0)))
  (make-array +chunk-size+ :initial-element nil))

;;; struct definitions

(defstruct (vector-trie (:conc-name vt-))
  (count nil :type fixnum)
  (shift nil :type fixnum)
  (root nil :type vector-node)
  (tail nil :type (simple-array t)))

(defstruct (vector-node (:conc-name vn-))
  (edit nil :type (or null atomic-reference) :read-only t)
  (array (make-array-chunk) :type (simple-array t) :read-only t))

(defstruct (persistent-vector (:conc-name pv-)
			      (:include vector-trie))
  (meta nil))

(defstruct (transient-vector (:conc-name tv-)
			     (:include vector-trie)))

;;; variables and constants

(defvar *no-edit* (make-atomic-reference :val nil))
(defvar *empty-node* (make-vector-node :edit *no-edit*))
(defvar *empty-vector* (make-persistent-vector :count 0 :shift +chunk-bit+ :root *empty-node* :tail (make-array 0)))

;;; generics

(defgeneric vec-conj (vector item))
(defgeneric vec-count (vector))
(defgeneric vec-assoc-n (vector n value))
(defgeneric vec-cons (vector val))
(defgeneric vec-val-at (vector n not-found))
(defgeneric vec-array-for (vector n))
(defgeneric vec-pop-last (vector))

;;; macros

(defmacro with-pv ((count shift root tail &optional meta) vec &body body)
  `(with-accessors ((,count vt-count)
		    (,shift vt-shift)
		    (,root vt-root)
		    (,tail vt-tail)
		    (,(or meta (gensym)) pv-meta))
      ,vec
     ,@body))

(defmacro with-vec ((count shift root tail) vec &body body)
  `(with-accessors ((,count vt-count)
		    (,shift vt-shift)
		    (,root vt-root)
		    (,tail vt-tail))
      ,vec
     ,@body))

;;; transient vector impl

(defun tv-ensure-editable (vec)
  (with-vec (count shift root tail) vec
    (unless (atomic-reference-val (vn-edit root))
      (error "Editting this vector is forbidden"))))

(defun tv-ensure-editable-node (vec node)
  (with-vec (count shift root tail) vec
    (if (eq (vn-edit root) (vn-edit node))
	node
	(make-vector-node :edit (vn-edit root)
			  :array (copy-seq (vn-array node))))))

(defun new-path (edit level node)
  (if (= level 0)
      node
      (let ((r (make-vector-node :edit edit)))
	(setf (aref (vn-array r) 0) (new-path edit (- level +chunk-bit+) node))
	r)))

(defun tv-push-tail (vec level parent tail-node)
  (with-vec (count shift root tail) vec
    (let* ((parent (tv-ensure-editable-node vec parent))
	   (subidx (logand +chunk-mask+ (ash (1- count) (- level)))))

      (setf (aref (vn-array parent) subidx)
	    (if (= level +chunk-bit+)
		tail-node
		(let ((child (aref (vn-array parent) subidx)))
		  (if child
		      (tv-push-tail vec (- level +chunk-bit+) child tail-node)
		      (new-path (vn-edit root) (- level +chunk-bit+) tail-node)))))
      parent)))

(defun tv-conj (vec val)
  (with-vec (count shift root tail) vec
    (tv-ensure-editable vec)
    (let ((i count))
      (if (< (- i (tv-tail-off vec)) +chunk-size+)
	  (progn
	    (setf (aref tail (logand i +chunk-mask+)) val)
	    (incf count))
	  ;;else 
	  (let ((new-tail (make-vector-node :edit (vn-edit root)
					    :array tail)))
	    (setf tail (make-array-chunk))
	    (setf (aref tail 0) val)

	    (if (> (ash count (- +chunk-bit+)) (ash 1 shift))
		(let ((new-root (make-vector-node :edit (vn-edit root))))
		  (setf (aref (vn-array new-root) 0) root)
		  (setf (aref (vn-array new-root) 1) (new-path (vn-edit root) shift new-tail))
		  (incf shift +chunk-bit+)
		  (setf root new-root))
		;;else
		(setf root (tv-push-tail vec shift root new-tail)))
	    (incf count)))))
  vec)

(defun tv-array-for (vec i)
  (with-vec (cnt shift root tail) vec
    (if (and (>= i 0) (< i cnt))
	(if (>= i (tv-tail-off vec))
	    tail
	    (loop for node = root then (aref (vn-array node) (logand (ash i (- level)) +chunk-mask+))
		  for level from shift above 0 by +chunk-bit+
		  finally (return (vn-array node))))
	(error "index out of bounds"))))

(defun tv-editable-array-for (vec i)
  (with-vec (cnt shift root tail) vec
    (if (and (>= i 0) (< i cnt))
	(if (>= i (tv-tail-off vec))
	    tail
	    (loop for node = root then (aref (vn-array (tv-ensure-editable-node vec node))
					     (logand (ash i (- level)) +chunk-mask+))
		  for level from shift above 0 by +chunk-bit+
		  finally (return (vn-array node))))
	(error "index out of bounds"))))

(defun tv-nth (vec i)
  (tv-ensure-editable vec)
  (aref (tv-array-for vec i)
	(logand i +chunk-mask+)))

(defun tv-val-at (vec i not-found)
  (tv-ensure-editable vec)
  (if (and (>= i 0) (< i (tv-count vec)))
      (tv-nth vec i)
      not-found))

(defun pv-make-iterator (vec &optional (start 0) (end nil))
  (with-pv (count shift root tail) vec
    (let ((i start)
	  (base (- start (mod start +chunk-size+)))
	  (array (if (< start count) (pv-array-for vec start) nil))
	  (r-end (or end count)))
      (lambda ()
	(when (< i r-end)
	  (when (= (- i base) +chunk-size+)
	    (setf array (pv-array-for vec i))
	    (incf base +chunk-size+))
	  (let ((element (aref array (logand i +chunk-mask+))))
	    (incf i)
	    (values t element)))))))

(defmethod vec-count ((vec transient-vector))
  (tv-count vec))

(defmethod vec-cons ((vec transient-vector) val)
  (tv-conj vec val))

(defmethod vec-val-at ((vec transient-vector) n not-found)
  (tv-val-at vec n not-found))

(defmethod vec-array-for ((vec transient-vector) i)
  (tv-array-for vec i))

(defmethod vec-assoc-n ((vec transient-vector) n val)
  (error "todo")
  )


;;; persistent vector impl

(defun editable-root (node)
  (make-vector-node :edit (make-atomic-reference :val t)
		    :array (copy-seq (vn-array node))))

(defun editable-tail (array)
  (let ((ret (make-array-chunk)))
    (array-copy array 0 ret 0 (length array))
    ret))

(defun pv-as-transient (vec)
  (with-pv (count shift root tail) vec
    (make-transient-vector :count count :shift shift
			   :root (editable-root root)
			   :tail (editable-tail tail))))

(defun tail-off (count)
  (if (< count +chunk-size+)
      0
      (ash (ash (1- count) (- +chunk-bit+)) +chunk-bit+)))

(defun tv-tail-off (vec)
  (tail-off (tv-count vec)))

(defun tv-as-persistent (vec)
  (with-vec (count shift root tail) vec
    (tv-ensure-editable vec)
    (setf (atomic-reference-val (vn-edit root)) nil)
    (let* ((len (- count (tv-tail-off vec)))
	   (shorter-tail (make-array len)))
      (array-copy tail 0 shorter-tail 0 len)
      (make-persistent-vector :count count
			      :shift shift
			      :root root
			      :tail shorter-tail))))

(defun create-persistent-vector (&rest items)
  (let ((len (length items)))
    (if (<= len +chunk-size+)
	(make-persistent-vector :count len :shift +chunk-bit+ :root *empty-node*
				:tail (make-array len :initial-contents items))
	(loop for ret = (pv-as-transient *empty-vector*)
		then (tv-conj ret item)
	      for item in items
	      finally (return (tv-as-persistent ret))))))

(defun pv-tail-off (vec)
  (tail-off (pv-count vec)))

(defun pv-push-tail (vec level parent tail-node)
  (with-pv (cnt shift root tail)
	   vec
    (let ((subidx (logand (ash (1- cnt) (- level)) +chunk-mask+))
	  (ret (make-vector-node :edit (vn-edit parent)
				 :array (copy-seq (vn-array parent))))
	  node-to-insert)
      (cond ((= level +chunk-bit+)
	     (setf node-to-insert tail-node))
	    (t (let ((child (aref (vn-array parent) subidx)))
		 (setf node-to-insert (if child
					  (pv-push-tail vec
							(- level +chunk-bit+)
							child tail-node)
					  (new-path (vn-edit root)
						    (- level +chunk-bit+) tail-node))))))
      (setf (aref (vn-array ret) subidx) node-to-insert)
      ret)))

(defun pv-cons (vec val)
  (with-pv (cnt shift root tail meta)
	   vec
    (if (and (< (length tail) +chunk-size+)
	     (< (- cnt (pv-tail-off vec)) +chunk-size+))
	(let ((new-tail (make-array (1+ (length tail)) :initial-element nil)))
	  (array-copy tail 0 new-tail 0 (length tail))
	  (setf (aref new-tail (length tail)) val)
	  (make-persistent-vector :meta meta 
				  :count (1+ cnt)
				  :shift shift
				  :root root
				  :tail new-tail))
	(let (new-root
	      (tail-node (make-vector-node :edit (vn-edit root) :array tail))
	      (new-shift shift))
	  (if (> (ash cnt (- +chunk-bit+)) (ash 1 shift))
	      (progn
		(setf new-root (make-vector-node :edit (vn-edit root)))
		(setf (aref (vn-array new-root) 0) root)
		(setf (aref (vn-array new-root) 1)
		      (new-path (vn-edit root) shift tail-node))
		(incf new-shift +chunk-bit+))
	      (progn
		(setf new-root (pv-push-tail vec shift root tail-node))))
	  (make-persistent-vector :meta meta :count (1+ cnt) :shift new-shift :root new-root :tail (vector val))))))

(defun pv-do-assoc (level node i val)
  (let ((ret (make-vector-node :edit (vn-edit node) :array (copy-seq (vn-array node)))))
    (if (= level 0)
	(setf (aref (vn-array ret) (logand i +chunk-mask+)) val)
	(let ((subidx (logand (ash i (- level)) +chunk-mask+)))
	  (setf (aref (vn-array ret) subidx)
		(pv-do-assoc (- level +chunk-bit+) (aref (vn-array node) subidx) i val))))
    ret))

(defun pv-assoc-n (vec i val)
  (with-pv (cnt shift root tail meta)
	   vec
    (cond ((and (>= i 0) (< i (pv-count vec)))
	   (if (>= i (pv-tail-off vec))
	       (let ((new-tail (copy-seq tail)))
		 (setf (aref new-tail (logand i +chunk-mask+)) val)
		 (make-persistent-vector :meta meta :count cnt
					 :shift shift :root root :tail new-tail))
	       
	       (make-persistent-vector :meta meta :count cnt
				       :shift shift :root (pv-do-assoc shift root i val)
				       :tail tail)))
	  ((= i cnt)
	   (pv-cons vec val))
	  
	  (t (error "index out of bounds")))))

(defun pv-array-for (vec i)
  (with-pv (count shift root tail) vec
    (if (and (>= i 0) (< i count))
	(if (>= i (pv-tail-off vec))
	    tail
	    (loop for node = root then (aref (vn-array node) (logand (ash i (- level)) +chunk-mask+))
		  for level = shift then (- level +chunk-bit+)
		  while (> level 0)
		  finally (return (vn-array node))))
	(error "index out of bounds"))))

(defun pv-nth (vec i)
  (aref (pv-array-for vec i) (logand i +chunk-mask+)))

(defun pv-nth-safe (vec i not-found)
  (if (and (>= i 0) (< i (pv-count vec)))
      (pv-nth vec i)
      not-found))

(defun pv-pop-tail (vec level node)
  (with-pv (count shift root tail) vec
    (let ((subidx (logand (ash (- count 2) (- level)) +chunk-mask+)))
      (cond ((> level 5)
	     (let ((new-child (pv-pop-tail vec (- level +chunk-bit+) (aref (vn-array node) subidx))))
	       (if (and (null new-child) (= 0 subidx))
		   nil
		   (let* ((new-array (copy-seq (vn-array node))))
		     (setf (aref new-array subidx) new-child)
		     (make-vector-node :edit (vn-edit root)
				       :array new-array)))))
	    ((= subidx 0) nil)
	    (t
	     (let ((new-array (copy-seq (vn-array node))))
	       (setf (aref new-array subidx) nil)
	       (make-vector-node :edit (vn-edit root)
				       :array new-array)))))))

(defun pv-pop (vec)
  (with-pv (count shift root tail meta) vec
    (cond ((= 0 count) (error "can't pop an empty vector"))
	  ((= 1 count) *empty-vector*)

	  ((> (- count (pv-tail-off vec)) 1)
	   (let* ((new-len (1- count))
		  (new-tail (make-array new-len)))
	     (array-copy tail 0 new-tail 0 new-len)
	     (make-persistent-vector :count new-len
				     :shift shift
				     :root root
				     :tail new-tail
				     :meta meta)))

	  (t
	   (let* ((new-tail (pv-array-for vec (- count 2)))
		  (new-root (or (pv-pop-tail vec shift root) *empty-node*))
		  (new-shift shift))
	     (when (and (> shift +chunk-bit+) (null (aref (vn-array new-root) 1)))
	       (setf new-root (aref (vn-array new-root) 0)
		     new-shift (- new-shift 5)))
	     (make-persistent-vector :count (1- count)
				     :shift new-shift
				     :root new-root
				     :tail new-tail
				     :meta meta))))))

(defmethod vec-count ((vec persistent-vector))
  (pv-count vec))

(defmethod vec-cons ((vec persistent-vector) val)
  (pv-cons vec val))

(defmethod vec-val-at ((vec persistent-vector) n not-found)
  (pv-nth-safe vec n not-found))

(defmethod vec-assoc-n ((vec persistent-vector) n val)
  (pv-assoc-n vec n val))

(defmethod vec-array-for ((vec persistent-vector) i)
  (pv-array-for vec i))

(defmethod vec-pop-last ((vec persistent-vector))
  (pv-pop vec))

;; vector-trie impl

(defun vec-make-iterator (vec &optional (start 0) (end nil))
  (with-vec (count shift root tail) vec
    (let ((i start)
	  (base (- start (mod start +chunk-size+)))
	  (array (if (< start count) (vec-array-for vec start) nil))
	  (r-end (or end count)))
      (lambda ()
	(when (< i r-end)
	  (when (= (- i base) +chunk-size+)
	    (setf array (vec-array-for vec i))
	    (incf base +chunk-size+))
	  (let ((element (aref array (logand i +chunk-mask+))))
	    (incf i)
	    (values t element)))))))

(defmethod print-object ((vec vector-trie) stream)
  (write-char #\[ stream)
  (loop with itr = (vec-make-iterator vec)
	for (remaining val) = (multiple-value-list (funcall itr))
	  then (list next-remaining next-val)
	while remaining
	for (next-remaining next-val) = (multiple-value-list (funcall itr))
	do (prin1 val stream)
	   (when next-remaining (write-char #\  stream)))
  (write-char #\] stream))

