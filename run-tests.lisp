#!/usr/bin/sbcl --script

(load (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :rove)
(ql:quickload :litterae)

(asdf:test-system :litterae)

