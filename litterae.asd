;;;; ========================================================================================== ;;;;
;;;; cl-litterae.asd                                                                            ;;;;
;;;; ========================================================================================== ;;;;

(asdf:defsystem #:litterae
  :description "Beautiful documentation generation."
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :version "0.2.1"
  :depends-on (:docparser
               :lsx
               :3bmd
               :3bmd-ext-code-blocks
               :str
               :cl-yaml)
  
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "string" :depends-on ("config"))
                 (:file "templates/index" :depends-on ("string"))
                 (:file "main" :depends-on ("templates/index"))))))

(asdf:defsystem #:litterae/tests
  :description "Test system for Litterae"
  :author "Stefan Devai <stedevai@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:litterae :litterae-test-system :rove)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main")))))
