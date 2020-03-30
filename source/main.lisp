;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae)

(eval-when (:compile-toplevel)
  (lsx:enable-lsx-syntax))

(defparameter *index* nil)
(defparameter *system-name* nil)
(defparameter *asdf-system* nil)
(defparameter *symbols* nil)

(defun generate (system-name &key (path #P"doc/"))
  "Generates static HTML documentation for a `system-name'."
  (assert (symbolp system-name))
  (ql:quickload system-name)
  (setf *system-name* system-name)
  (setf *asdf-system* (asdf:find-system *system-name*))

  ;; Create *index* hash and silence output from docparser
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (setf *index* (docparser:parse system-name)))
  
  (build-symbols-hash)
  (generate-html path))

(defun g (system-name &key (path #P"doc/"))
  "Generates static HTML documentation for a `system-name'."
  (generate system-name :path path))

(defun build-symbols-hash ()
  "Stores in `*symbols*' a hash of hashes of lists: a hash of package names, which each value contains a hash of class names which, each value contains a list of node names."
  (setf *symbols* (make-hash-table))

  (docparser:do-packages (pkg *index*)
      ;; Add symbol names entries
      (let ((package-hash (or (gethash pkg *symbols*) (make-hash-table))))
        (docparser:do-nodes (node pkg)
          (push node
                (gethash (class-name (class-of node))
                         package-hash)))

        ;; Sort symbol name list
        (loop :for value :being :the :hash-values :of package-hash
              :do (sort value (lambda (node1 node2)
                                (string-lessp (docparser:node-name node1)
                                              (docparser:node-name node2)))))
        (setf (gethash pkg *symbols*) package-hash))))

(defun generate-html (path)
  "Generates HTML for the contents of a parsed system in `*symbols*'."
  (ensure-directories-exist path)
  (generate-html-index path))
  ;; (do-package-hashes
  ;;   (format t "PACKAGE: ~(~a~)~%" (docparser:package-index-name package))
  ;;   (do-node-lists package-hash
  ;;     (format t "  NODE TYPE: ~(~a~)~%" node-type)
  ;;     (mapcar (lambda (node) (format t "    ~(~a~): ~a~%"
  ;;                                    (docparser:node-name node)
  ;;                                    (class-of node)))
  ;;             node-list)
  ;;     (format t "~%"))))

(defun generate-html-index (path)
  "Generates index.html in `path'."
  (with-open-file (stream (merge-pathnames #P"index.html" path)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (lsx:render-object (make-index-template) stream)))

(defun make-index-template ()
  (make-instance 'index-template
                 :title (format nil "~(~a~)" *system-name*)
                 :description (asdf:system-description *asdf-system*)
                 :url (asdf:system-homepage *asdf-system*)
                 :body (generate-html-index-body)))
;                 :body (generate-html-index-body)))

(defun generate-html-index-body ()
  "Generates the body content for the index page."
  <div>
  {(html-hero)}
  {(html-sidebar)}
  {(lsx:make-danger-element :element (html-readme))}
  {(html-main)}
  </div>)

(defun html-hero ()
  <header>
  <h1>{(format nil "~a" *system-name*)}</h1>
  <p>{(asdf:system-description *asdf-system*)}</p>
  </header>)

(defun html-sidebar ()
  <aside>
  <nav>
      <h3>{(format nil "~a" *system-name*)}</h3>
      <ul>
        {(generate-list
          :child-list? t
          :elements (do-package-hashes (pkg package-hash)
            (list (docparser:package-index-name pkg)
                  (generate-list
                   :child-list? t
                   :element-format "~:(~a~)"
                   :elements (do-node-lists package-hash
                               (list (get-node-type-string node-type :plural? t)
                                     (generate-list
                                      :child-list? nil
                                      :elements (mapcar (lambda (node) (docparser:node-name node))
                                                        node-list))))))))}
      </ul>
  </nav>
  </aside>)

(defun html-readme ()
  (let* ((system-dir (pathname-directory
                     (nth 2 (multiple-value-list
                             (asdf:locate-system *asdf-system*)))))
         (files (directory (make-pathname :directory system-dir
                                          :name :wild
                                          :type :wild))))
    (loop :for fp :in files
          :when (string-equal "readme"
                              (format nil "~(~a~)" (pathname-name fp)))
            :do (return-from html-readme (read-markdown fp)))))

(defun read-markdown (filepath)
  (markdown.cl:parse-file filepath))

(defun html-main ()
  <main>
  And here goes the main content
  </main>)

(defun generate-list (&key elements (child-list? nil) (element-format "~(~a~)"))
  "Generate a list with `elements' and surrounds it with a <li> tag with `title'.
The type can be either :ul or :ol."
  (if child-list? 
      (mapcar (lambda (e) <li>{(format nil element-format (car e))} <ul>{(cdr e)}</ul></li>)
              elements)
      (mapcar (lambda (e) <li>{(format nil element-format e)}</li>)
              elements)))

(defun get-node-type-string (node-type &key (plural? nil))
  (case node-type
    (docparser:variable-node (if plural? "variables" "variable"))
    (docparser:class-node    (if plural? "classes"   "class"))
    (docparser:method-node   (if plural? "methods"   "method"))
    (docparser:macro-node    (if plural? "macros"    "macro"))
    (docparser:function-node (if plural? "functions" "function"))
    (otherwise (format nil "~(~a~)" node-type))))

(defmacro do-package-hashes ((pkg package-hash) &body body)
  "Iterates through the package hashes in `*index*'"
  `(loop :for ,pkg :being :the :hash-keys :of *symbols*
           :using (:hash-value ,package-hash)
         :collect (progn ,@body)))

(defmacro do-node-lists (package-hash &body body)
  `(loop :for node-type :being :the :hash-keys :of ,package-hash
           :using (:hash-value node-list)
         :collect (progn ,@body)))
