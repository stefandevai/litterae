;;;; ========================================================================================== ;;;;
;;;; test-system.lisp                                                                           ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:cl-user)
(defpackage #:litterae-test-system
  (:use :cl)
  (:documentation "docstring"))

(in-package #:litterae-test-system)

(defvar var1 nil
  "Docstring for var1.")

(defvar var2 nil
  "Docstring for var2.")

(defparameter par1 nil
  "Docstring for par1")

(defparameter par2 nil
  "Docstring for par1")

(defun func1 (arg1 arg2)
  "Docstring for func1."
  (format nil "~a~a~%" arg1 arg2))

(defun func2 (arg1 arg2)
  "Docstring for func2."
  (format nil "~a~a~%" arg1 arg2))

(defun func4 (arg1 arg2)
  "Docstring for func3."
  (format nil "~a~a~%" arg1 arg2))

(defun func3 (arg1 arg2)
  "Docstring for func3."
  (format nil "~a~a~%" arg1 arg2))

(defmacro macro1 (filepath)
  "Docstring for macro1"
  (format nil "~a~%" filepath))

(defvar var3 nil
  "Docstring for var3.")

(defmacro macro2 (&key (filepath #P"path/to/file"))
  "Docstring for macro2"
  (format nil "~a~%" filepath))

(defparameter par3 nil
  "Docstring for par3")

(defun func5 (arg1 arg2)
  "Docstring for func5."
  (format nil "~a~a~%" arg1 arg2))

(defvar var4 nil
  "Docstring for var4.")

(defun func6 (arg1 arg2)
  "Docstring for func6."
  (format nil "~a~a~%" arg1 arg2))

(defparameter par4 nil
  "Docstring for par4")
