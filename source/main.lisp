;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae)

;; Enable lsx syntax in compile time
(eval-when (:compile-toplevel)
  (lsx:enable-lsx-syntax))

(defparameter *index* nil
  "Holds raw information about a system provided by docparser library.")

(defparameter *system-name* nil
  "Holds system's name as symbol.")

(defparameter *asdf-system* nil
  "Hold's the system's asdf information.")

(defparameter *symbols* nil
  "Structured information about the system's symbols.")

(defmacro do-package-hashes ((pkg package-hash) &body body)
  "Iterates through the package hashes in `*index*'"
  `(loop :for ,pkg :being :the :hash-keys :of *symbols*
           :using (:hash-value ,package-hash)
         :collect (progn ,@body)))

(defmacro do-node-lists (package-hash &body body)
  "Iterates through lists of nodes in a `package-hash' contained in  `*index'."
  `(loop :for node-type :being :the :hash-keys :of ,package-hash
           :using (:hash-value node-list)
         :collect (progn ,@body)))

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
            :using (:hash-key key)
            :do (setf (gethash key package-hash) (sort value (lambda (node1 node2)
                              (string-lessp (docparser:node-name node1)
                                            (docparser:node-name node2))))))
      (setf (gethash pkg *symbols*) package-hash))))

(defun generate-html (path)
  "Generates HTML for the contents of a parsed system in `*symbols*'."
  (ensure-directories-exist path)
  (generate-html-index path))

(defun generate-html-index (path)
  "Generates index.html in `path'."
  (with-open-file (stream (merge-pathnames #P"index.html" path)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (lsx:render-object (make-index-template) stream)))

(defun make-index-template ()
  "Creates a object instance for `index-template'."
  (make-instance 'index-template
                 :title (format nil "~(~a~)" *system-name*)
                 :description (asdf:system-description *asdf-system*)
                 :url (asdf:system-homepage *asdf-system*)
                 :body (generate-html-index-body)))

(defun generate-html-index-body ()
  "Generates the body content for the index page."
  <div>
  {(html-hero)}
  {(html-sidebar)}
  {(lsx:make-danger-element :element (html-readme))}
  {(html-main)}
  </div>)

(defun html-hero ()
  "Generates the Hero HTML."
  <header>
  <h1>{(format nil "~a" *system-name*)}</h1>
  <p>{(asdf:system-description *asdf-system*)}</p>
  </header>)

(defun html-sidebar ()
  "Returns the sidebar used to navigate through the API."
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
  "Searches for a README file in the system's directory and returns it
as a HTML string."
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
  "Reads and parses a markdown file located in `filepath'."
  (with-output-to-string (out)
    (let ((3bmd-code-blocks:*code-blocks* t)
          (3bmd-code-blocks:*renderer* :pygments))

      (3bmd:parse-and-print-to-stream filepath out))))

(defun html-main ()
  "Returns the main API documentation content as a lsx object."
  <main>
      <h2>API Documentation</h2>
      {(do-package-hashes (pkg pkg-hash)
         (list
          (lsx:h "h3" '(("" . nil))
                 (list (format nil "Package: ~a"
                               (docparser:package-index-name pkg))))
          (do-node-lists pkg-hash
            (list
             (lsx:h "h4" '(("" . nil))
                    (list (format nil "~@(~a~)"
                                  (get-node-type-string node-type :plural? t))))
             (mapcar (lambda (node) (gen-html-node-item node pkg))
                     node-list)))))}
    </main>)

(defun gen-html-node-item (node pkg)
  "Returns each docparser node formated as HTML."
  <div>
  <h5>{(get-lambda-list node pkg)}</h5>
  <p>{(docparser:node-docstring node)}</p>
  </div>)

(defun get-lambda-list (node pkg)
  "If the node is of type operator-node, the function returns its lambda list.
Otherwise it returns the node-name as a string."
  ;; Temporarily binds *package* to the documented package so its prefix
  ;; doens't show in the lambda list tokens.
  (let ((*package* (find-package (docparser:package-index-name pkg))))
    (if (typep node 'docparser:operator-node)
        (format nil "~(~S~) ~a"
                (docparser:node-name node)
                (format-lambda-list (docparser:operator-lambda-list node)))
        (format nil "~(~S~)" (docparser:node-name node)))))

(defun format-lambda-list (lst)
  "Formats a lambda list `lst` and returns it as a string."
  (assert (listp lst))
  (format nil "~a"
   (mapcar
    (lambda (token)
      (case (type-of token)
        (cons (format-lambda-list token)) ; If = list, we call format-lambda-list recursively
        (pathname (format nil "~S" token)) ; If = pathname, we return it as a string
        (otherwise (format nil "~(~S~)" token)))) ; Otherwise we return it as a lowercase string
    lst)))

(defun generate-list (&key elements (child-list? nil) (element-format "~(~a~)"))
  "Generates a list with `elements'. If `child-list?' is true, it uses the first element
of `elements' (car) as the title and the other elements as lines of a new list. If child-list?
is false, then each element in `elements' will be a list.
element-format allows to customize how the element will be printed."
  (if child-list? 
      (mapcar (lambda (e) <li>{(format nil element-format (car e))} <ul>{(cdr e)}</ul></li>)
              elements)
      (mapcar (lambda (e) <li>{(format nil element-format e)}</li>)
              elements)))

(defun get-node-type-string (node-type &key (plural? nil))
  "Return a string of `node-type' for headers. If `plural' is non nil, it returns its plural version."
  (case node-type
    (docparser:variable-node (if plural? "variables" "variable"))
    (docparser:class-node    (if plural? "classes"   "class"))
    (docparser:method-node   (if plural? "methods"   "method"))
    (docparser:macro-node    (if plural? "macros"    "macro"))
    (docparser:function-node (if plural? "functions" "function"))
    (otherwise (format nil "~(~a~)" node-type))))

