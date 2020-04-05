;;;; ========================================================================================== ;;;;
;;;; main.lisp                                                                                  ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae)

;; Enable lsx syntax in compile time
(eval-when (:compile-toplevel)
  (lsx:enable-lsx-syntax))

(defparameter *docstrings-as-markdown?* t
  "If true, docstrings will be parsed as markdown, otherwise it uses the string as it is.")

(defparameter *index* nil
  "Holds raw information about a system provided by docparser library.")

(defparameter *system-name* nil
  "Holds system's name as symbol.")

(defparameter *asdf-system* nil
  "Hold's the system's asdf information.")

(defparameter *symbols* nil
  "Structured information about the system's symbols.")

(defmacro do-package-hashes ((pkg pkg-hash) &body body)
  "Iterates through the package hashes in `*index*'"
  `(loop :for ,pkg :being :the :hash-keys :of *symbols*
           :using (:hash-value ,pkg-hash)
         :collect (progn ,@body)))

(defmacro do-node-lists ((pkg-hash) &body body)
  "Iterates through lists of nodes in a `package-hash' contained in  `*index'."
  `(loop :for node-type :being :the :hash-keys :of ,pkg-hash
           :using (:hash-value node-list)
         :collect (progn ,@body)))

(defun generate (system-name &key (path #P"doc/"))
  "Generates static HTML documentation for a `system-name'."
  (initialize-system-information system-name)
  (build-symbols-hash)
  (generate-html path))

(defun g (system-name &key (path #P"doc/"))
  "Generates static HTML documentation for a `system-name'."
  (generate system-name :path path))

(defun initialize-system-information (system-name)
  (assert (symbolp system-name))
  (setf *system-name* system-name)
  (setf *asdf-system* (asdf:find-system *system-name*))
;  (setf *index* (docparser:parse system-name))
  ;; Create *index* hash and silence output from docparser
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (setf *index* (docparser:parse system-name))))

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

  <main>
  <div class="main-content">
  <div class="readme">
  {(lsx:make-danger-element :element (html-readme))}
  </div>
  
  {(html-api-docs)}
  </div>
  </main>
  </div>)

(defun html-hero ()
  "Generates the Hero HTML."
  <header class="hero">
  <h1>{(format nil "~a" *system-name*)}</h1>
  <p>{(asdf:system-description *asdf-system*)}</p>
  </header>)

(defun html-sidebar ()
  "Returns the sidebar used to navigate through the API."
  <aside>
  <nav class="nav-table-of-contents">
  <h5>Table of Contents ⟶</h5>
  <ul>
  {(let ((id 0))
     (generate-list
      :child-list? t
      :elements
      (do-package-hashes (pkg pkg-hash)
        (list
         :name (docparser:package-index-name pkg)
         :child-list (generate-list
                      :child-list? t
                      :element-format "~:(~a~)"
                      :elements
                      (do-node-lists (pkg-hash)
                        (list
                         :name (get-node-type-string node-type :plural? t)
                         :child-list (generate-list
                                      :child-list? nil
                                      :elements
                                      (mapcar (lambda (node)
                                                (progn
                                                  (incf id)
                                                  (list
                                                   :id id
                                                   :name (docparser:node-name node))))
                                              node-list)))))))))}
  </ul>
  </nav>
  </aside>)

(defun generate-list (&key elements (child-list? nil) (element-format "~(~a~)"))
  "Generates a list with `elements'. If `child-list?' is true, it uses the first element
of `elements' (car) as the title and the other elements as lines of a new list. If child-list?
is false, then each element in `elements' will be a list.
element-format allows to customize how the element will be printed."
  (if child-list? 
      (mapcar (lambda (e)
                <li><a href="#">{(format nil element-format (getf e :name))}</a><ul>{(getf e :child-list)}</ul></li>)
              elements)
      
      (mapcar (lambda (e)
                <li><a href={(str:concat "#" (generate-id e))}>{(format nil element-format (getf e :name))}</a></li>)
              elements)))

(defun generate-id (element)
  "Generates an id string for html given an element in the format `(list :id 0 :name some-name)`."
  (str:replace-all
   "/" "-"
   (str:concat (format nil "~(~S~)" (getf element :name))
               "-"
               (write-to-string (getf element :id)))))

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

(defun html-api-docs ()
  "Returns the main API documentation content as a lsx object."
  <div class="api-docs">
  <h1>API Documentation</h1>
  {(let ((id 0))
     (do-package-hashes (pkg pkg-hash)
       (list
        (lsx:h "h3" '(("" . nil))
               (list (format nil "Package: ~a"
                             (docparser:package-index-name pkg))))
        (do-node-lists (pkg-hash)
          (list
           (lsx:h "h4" '(("" . nil))
                  (list (format nil "~@(~a~)"
                                (get-node-type-string node-type :plural? t))))
           (mapcar (lambda (node)
                     (progn
                       (incf id)
                       (gen-html-node-item node id pkg)))
                   node-list))))))}
  </div>)

(defun gen-html-node-item (node id pkg)
  "Returns each docparser node formated as HTML."
  <div>
  <h5 id={(generate-id (list :id id :name (docparser:node-name node)))}>
  {(lsx:make-danger-element :element (get-lambda-list node pkg))}
  </h5>
  <p>
  {(if *docstrings-as-markdown?*
       (lsx:make-danger-element
        :element (parse-markdown-docstring
                  (docparser:node-docstring node)))
       <p>{(docparser:node-docstring node)}</p>)}
  </p>
  </div>)

(defun parse-markdown-docstring (docstring)
  "Returns a **docstring** in markdown format as a HTML string."
  (when docstring 
    (with-output-to-string (out)
      (let ((3bmd-code-blocks:*code-blocks* t)
            (3bmd-code-blocks:*renderer* :pygments))
        (3bmd:parse-string-and-print-to-stream docstring out)))))

(defun get-lambda-list (node pkg)
  "If the node is of type operator-node, the function returns its lambda list.
Otherwise it returns the node-name as a string."
  ;; Temporarily binds *package* to the documented package so its prefix
  ;; doens't show in the lambda list tokens.
  (let ((*package* (find-package (docparser:package-index-name pkg))))
    (if (and (typep node 'docparser:operator-node) (docparser:operator-lambda-list node))
        (format nil "~(~S~) ~a"
                (docparser:node-name node)
                (format-lambda-list (docparser:operator-lambda-list node)))
        (format nil "~(~S~)" (docparser:node-name node)))))

(defun format-lambda-list (lst)
  "Formats a lambda list `lst` and returns it as a string."
  (assert (listp lst))
  (format-lambda-list-html
   (reduce
    (lambda (t1 t2) (str:concat t1 " " t2))
    (mapcar
     (lambda (token)
       (let ((token-string 
               (case (type-of token)
                 (cons (format-lambda-list token)) ; If = list, we call format-lambda-list recursively
                 (pathname (format nil "~S" token)) ; If = pathname, we return it as a string
                 (otherwise (format nil "~(~S~)" token))))) ; Otherwise we return it as a lowercase string
         (case (char token-string 0)
           (#\: (str:concat "<span class=\"keyword\">" token-string "</span>"))
           (#\& (str:concat "<span class=\"symbol\">" token-string "</span>"))
           (otherwise token-string))))
     lst))))

(defun format-lambda-list-html (lambda-string)
  "Returns a lambda list string with proper formated HTML."
  (str:concat "<span class=\"lambda-parameters\"><span class=\"parenthesis\">(</span> "
              (str:replace-all "(" "<span class=\"parenthesis\">(</span> "
                               (str:replace-all ")" " <span class=\"parenthesis\">)</span>"
                                                lambda-string))
              " <span class=\"parenthesis\">)</span></span>"))

(defun get-node-type-string (node-type &key (plural? nil))
  "Return a string of `node-type' for headers. If `plural' is non nil, it returns its plural version."
  (case node-type
    (docparser:variable-node (if plural? "variables" "variable"))
    (docparser:class-node    (if plural? "classes"   "class"))
    (docparser:method-node   (if plural? "methods"   "method"))
    (docparser:macro-node    (if plural? "macros"    "macro"))
    (docparser:function-node (if plural? "functions" "function"))
    (otherwise (format nil "~(~a~)" node-type))))

