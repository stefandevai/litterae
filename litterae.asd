;;;; ========================================================================================== ;;;;
;;;; cl-litterae.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:litterae
  :description "Beautiful documentation generation."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:docparser :lsx :markdown.cl)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "string" :depends-on ("package"))
                 (:file "templates/index" :depends-on ("string"))
                 (:file "main" :depends-on ("templates/index")))))
  
  :in-order-to ((test-op (test-op :litterae/tests))))

(asdf:defsystem #:litterae/tests
  :description "Test system for Litterae"
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:litterae :rove)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :rove '#:run c)))