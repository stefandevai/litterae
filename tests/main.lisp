;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae/tests)

(defun alphabetical-list? (lst)
  "Returns t if strings in lst are in alphabetical order, otherwise returns nil."
  (let ((string-list (mapcar (lambda (e) (string (docparser:node-name e))) lst)))
    (every #'string-lessp string-list (cdr string-list))))

(setup
  (litterae::initialize-system-information :litterae-test-system)
  (litterae::build-symbols-hash))

(deftest symbol-hash-creation
  (testing "system is loaded properly"
    (ok (symbolp litterae::*system-name*))
    (ok (equal litterae::*system-name* :litterae-test-system))

    (let ((fn-nodes (docparser:query litterae::*index*
                                     :package-name "LITTERAE-TEST-SYSTEM"
                                     :class 'docparser:function-node)))
      (ng (alphabetical-list? (coerce fn-nodes 'list)))
      (ok (= (length fn-nodes)
             6)))

    (let ((macro-nodes (docparser:query litterae::*index*
                                     :package-name "LITTERAE-TEST-SYSTEM"
                                     :class 'docparser:macro-node)))
      (ok (= (length macro-nodes)
             2))))

  (testing "hashmap is created properly"
    (litterae::do-package-hashes (pkg pkg-hash)
      (litterae::do-node-lists pkg-hash
        (when (equal litterae::node-type 'docparser:macro-node)
          (ok (= 2 (length litterae::node-list))))
        (when (equal litterae::node-type 'docparser:function-node)
          (ok (= 6 (length litterae::node-list)))))))
  
  (testing "node lists kept in alphabetical order"
    (litterae::do-package-hashes (pkg pkg-hash)
      (litterae::do-node-lists pkg-hash
        (when (equal litterae::node-type 'docparser:function-node)
          (ok (alphabetical-list? litterae::node-list)))))))

(deftest parse-docstring-as-markdown
  (testing "basic markdown style"
    (ok (string= (litterae::parse-markdown-docstring "***test***")
                 (format nil "<p><strong><em>test</em></strong></p>~%")))))






