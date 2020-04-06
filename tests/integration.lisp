;;;; ========================================================================================== ;;;;
;;;; integration.lisp                                                                           ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae/tests)

(defvar *test-data-path* "tests/test-data/"
  "Path for the system and the config file used in tests.")

(deftest documentation-generation
  (testing "generating docs for the test system"
    (uiop:run-program (concatenate 'string "rm -rf " *test-data-path* "doc"))
    
    (litterae:g :litterae-test-system
                :path (merge-pathnames #P"doc/" *test-data-path*)
                :config (merge-pathnames #P"test-config.yml" *test-data-path*))
    (ok (probe-file (merge-pathnames #P"doc/index.html" *test-data-path*)))
    
    (uiop:run-program (concatenate 'string "rm -rf " *test-data-path* "doc"))))


