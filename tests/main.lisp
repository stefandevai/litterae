;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae/tests)

(setup
  (litterae::initialize-system-information :litterae-test-system)
  (litterae::build-symbols-hash))

(deftest symbol-hash-creation
  (testing "system name is kept in variable"
    (ok (symbolp litterae::*system-name*))
    (ok (equal litterae::*system-name* :litterae-test-system))

    (let ((fn-nodes (docparser:query litterae::*index*
                                     :package-name "LITTERAE-TEST-SYSTEM"
                                     :class 'docparser:function-node)))
      (ok (= (length fn-nodes)
             6)))))

