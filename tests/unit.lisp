;;;; ========================================================================================== ;;;;
;;;; unit.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae/tests)

(defun alphabetical-list? (lst)
  "Returns t if strings in lst are in alphabetical order, otherwise returns nil."
  (let ((string-list (mapcar (lambda (e) (string (docparser:node-name e))) lst)))
    (every #'string-lessp string-list (cdr string-list))))

(defparameter *docparser-package* nil
  "Holds the test package docparser object.")

(setup
  (litterae::initialize-system-information :litterae-test-system)
  (litterae::build-symbols-hash)
  (docparser:do-packages (pkg litterae::*index*)
    (setq *docparser-package* pkg)))


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
      (litterae::do-node-lists (pkg-hash)
        (when (equal litterae::node-type 'docparser:macro-node)
          (ok (= 2 (length litterae::node-list))))
        (when (equal litterae::node-type 'docparser:function-node)
          (ok (= 6 (length litterae::node-list)))))))
  
  (testing "node lists kept in alphabetical order"
    (litterae::do-package-hashes (pkg pkg-hash)
      (litterae::do-node-lists (pkg-hash)
        (when (equal litterae::node-type 'docparser:function-node)
          (ok (alphabetical-list? litterae::node-list)))))))

(deftest parse-docstring-as-markdown
  (testing "basic markdown style"
    (ok (string= (litterae::parse-markdown-docstring "***test***")
                 (format nil "<p><strong><em>test</em></strong></p>~%")))))

(deftest header-id-generation
    (testing "generates correct id in hrefs"
      (ok (string= (lsx:render-object
                    (litterae::generate-list
                     :elements '((:id 0 :name A) (:id 1 :name B))) nil)                   
                   (format nil "~a~%~a~%" "<li><a href=\"#litterae-tests::a-0\">a</a></li>"
                           "<li><a href=\"#litterae-tests::b-1\">b</a></li>")))))

(deftest load-configuration
  (testing "loads custom configuration"
    (ok (litterae::load-config))
    (ok litterae::*docstrings-as-markdown?*)
    (ok (litterae::load-config #P"tests/test-data/test-config.yml"))
    (ok (null litterae::*docstrings-as-markdown?*))))

(deftest lambda-lists
  (testing "signal non lists"
    (signals (litterae::format-lambda-list #P"test"))
    (signals (litterae::format-lambda-list 3))
    (signals (litterae::format-lambda-list "test")))

  (testing "lambda lists generation"
    (let ((variable-node (elt (docparser:query litterae::*index* :symbol-name :var1) 0))
          (function-node (elt (docparser:query litterae::*index* :symbol-name :func1) 0)))
      (ok (string= "var1"
                   (litterae::get-lambda-list variable-node *docparser-package*)))
      (ok (string= "func1 <span class=\"lambda-parameters\"><span class=\"parenthesis\">(</span> arg1 arg2 <span class=\"parenthesis\">)</span></span>"
                   (litterae::get-lambda-list function-node *docparser-package*)))))

  (testing "correct formatting of lambda lists"
    (ok (string= "<span class=\"lambda-parameters\"><span class=\"parenthesis\">(</span> a <span class=\"keyword\">:b</span> c <span class=\"symbol\">&d</span> e #P\"Path\" <span class=\"parenthesis\">)</span></span>"
                 (let ((*package* (find-package :litterae/tests)))
                   (litterae::format-lambda-list '(a :b c &d e #P"Path")))))))


