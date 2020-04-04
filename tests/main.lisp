;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae/tests)

(deftest dummy-test
    (format t "~a~%" (docparser:dump (docparser:parse :ibidem)))
  (testing "dummy"
    (ok (equal 0
               0))))
