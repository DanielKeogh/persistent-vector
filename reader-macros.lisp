;;;; reader-macros

(in-package :persistent-vector)

;; constants

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])

;; vars

(defvar *previous-readtables* nil)

;; vector-reader

(defun vector-reader (stream char)
  (declare (ignore char))
  (let ((lst (read-delimited-list +right-bracket+ stream nil)))
    `(persistent-vector::create-persistent-vector ,@lst)))

(defun no-matching-bracket (stream char)
  (declare (ignore stream char))
  (error "No matching [ for ]"))

;; enable/disable

(defmacro enable-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     (setf *readtable* (copy-readtable))
     (set-macro-character +left-bracket+ 'vector-reader)
     (set-macro-character +right-bracket+ 'no-matching-bracket)))

(defmacro disable-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (pop *previous-readtables*))))
