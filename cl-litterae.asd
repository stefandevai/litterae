;;;; ========================================================================================== ;;;;
;;;; cl-litterae.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:cl-litterae
  :description "Beautiful documentation generation."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.0.0"
  :serial t
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "main"))))
  :in-order-to ((test-op (test-op :cl-litterae/tests))))

(asdf:defsystem #:cl-litterae/tests
  :description "Test system for cl-litterae"
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :depends-on (:cl-litterae :rove)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :rove '#:run c)))
