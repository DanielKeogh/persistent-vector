;;;; utils.lisp

(in-package :persistent-vector)

(defstruct atomic-reference (val nil))

(defun required-argument (name)
  (error "Required argument ~@[~S~] missing." name))

(defun equiv (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (cl:equal v1 v2))

(defun array-copy (src src-pos dest dest-start length)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array t (*)) src dest)
	   (type fixnum src-pos dest-start length))
  (loop for cnt fixnum from 0 below length
	for src-index fixnum from src-pos
	for dest-index fixnum from dest-start
	do (setf (aref dest dest-index) (aref src src-index))))
