(asdf:defsystem #:coalton-rope
  :description "Immutable Ropes for Coalton"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (#:coalton)
  :components ((:file "rope"))
  :in-order-to ((test-op (test-op #:coalton-rope/test))))

(asdf:defsystem #:coalton-rope/test
  :depends-on (#:coalton-rope #:coalton/testing)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "basic"))))
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call '#:coalton-rope/test '#:run-tests)))
