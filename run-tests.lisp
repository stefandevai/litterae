#!/usr/bin/sbcl --script

(load (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :rove)
(ql:quickload :cl-litterae)

(asdf:test-system :cl-litterae)

