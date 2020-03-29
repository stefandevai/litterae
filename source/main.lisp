;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae)

(defparameter *index* nil)
(defparameter *symbols* (make-hash-table))

(defun generate (system-name)
  "Generates static HTML documentation for a `system-name'."
  (assert (symbolp system-name))

  ;; Create *index* hash and silence output from docparser
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (setf *index* (docparser:parse system-name)))
  
  (build-symbols-hash)
  (generate-html))
  
(defun build-symbols-hash ()
  "Stores in `*symbols*' a hash of hashes of lists: a hash of package names, which each value contains a hash of class names which, each value contains a list of node names."
  
  ;; NOTE: Only for debug purposes
  (setf *symbols* (make-hash-table))

  (docparser:do-packages (package *index*)
      ;; Add symbol names entries
      (let ((package-hash (or (gethash package *symbols*) (make-hash-table))))
        (docparser:do-nodes (node package)
          ;; (push (docparser:node-name node)
          (push node
                (gethash (class-name (class-of node))
                         package-hash)))

        ;; Sort symbol name list
        (loop :for value :being :the :hash-values :of package-hash
              :do (sort value (lambda (node1 node2)
                                (string-lessp (docparser:node-name node1)
                                              (docparser:node-name node2)))))
        (setf (gethash package *symbols*) package-hash))))

(defun generate-html ()
  "Generates HTML for the contents of a parsed system in `*symbols*'."
  (with-package-hashes
    (format t "PACKAGE: ~(~a~)~%" (docparser:package-index-name package))
    (with-node-lists package-hash
      (format t "  NODE TYPE: ~(~a~)~%" node-type)
      (mapcar (lambda (node) (format t "    ~(~a~)~%"
                                     (docparser:node-name node)))
              node-list)
      (format t "~%"))))

(defmacro with-package-hashes (&body body)
  "Iterates through the package hashes in `*index*'"
  `(loop :for package :being :the :hash-keys :of *symbols*
           :using (:hash-value package-hash)
         :do (progn ,@body)))

(defmacro with-node-lists (package-hash &body body)
  `(loop :for node-type :being :the :hash-keys :of ,package-hash
           :using (:hash-value node-list)
         :do (progn ,@body)))

(loop :for key :being :the :hash-keys :of *symbols*
        :using (hash-value value)
        :do (format t "~&The value associated with the key ~S is ~S~%" key value)
      :do (loop :for k2 :being :the hash-keys :of value
                  :using (hash-value v2)
                :do (format t "Key is ~S, value is ~S~%" k2 v2)))

