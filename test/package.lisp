(defpackage #:coalton-rope/test
  (:use #:coalton #:coalton-prelude #:coalton-testing)
  (:local-nicknames
   (#:rope #:coalton-rope))
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
  (:export #:run-tests))
(in-package #:coalton-rope/test)

(fiasco:define-test-package #:coalton-rope/fiasco-test-package)
(coalton-fiasco-init #:coalton-rope/fiasco-test-package)

(cl:defun run-tests (cl:&optional interactive)
  (fiasco:run-package-tests
   :packages '(#:coalton-rope/fiasco-test-package)
   :interactive interactive))
