;;;; tests/package.lisp

(defpackage #:persistent-vector-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:pv #:persistent-vector))
  (:export #:run!
	   #:all-tests))
