#!/usr/bin/sbcl --script

(load #P"~/.quicklisp/setup.lisp")
(ql:quickload :rove)
(ql:quickload :cl-litterae)

(asdf:test-system :cl-litterae)

