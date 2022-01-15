;;;; tests/package.lisp

(defpackage #:persistent-vector-tests
  (:use #:cl #:fiveam)
  (:nicknames #:pv-tests)
  (:export #:run!
           #:all-tests))
