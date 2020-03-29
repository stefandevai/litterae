;;;; ========================================================================================== ;;;;
;;;; package.lisp                                                                               ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:cl-user)
(defpackage #:cl-litterae/tests
  (:use #:cl #:rove #:litterae)
  (:shadowing-import-from #:rove
                          #:*debug-on-error*))
