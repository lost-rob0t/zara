(defpackage :zara/tests
  (:use :cl :fiveam)
  (:export #:run-tests))

(in-package :zara/tests)

(def-suite zara-suite)
(in-suite zara-suite)

(defun run-tests ()
  (run! 'zara-suite))
