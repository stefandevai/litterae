;;;; ========================================================================================== ;;;;
;;;; config.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae)

(defparameter *index* nil
  "Holds raw information about a system provided by docparser library.")

(defparameter *system-name* nil
  "Holds system's name as symbol.")

(defparameter *asdf-system* nil
  "Hold's the system's asdf information.")

(defparameter *symbols* nil
  "Structured information about the system's symbols.")

(defparameter *config-filename* ".litterae.yml"
  "Holds the default config filename.")

(defparameter *docstrings-as-markdown?* t
  "If true, docstrings will be parsed as markdown, otherwise it uses the string as it is.")

(defmacro set-single-config (parameter keyword hash-table)
  "Sets the value of **parameter** to the value of **keyword** in **hash-table** only if the keyword exists."
  (let ((value (gensym)))
    `(multiple-value-bind (,value exists?) (gethash ,keyword ,hash-table)
       (when exists? (setf ,parameter ,value)))))

(defun load-config (&optional (filepath nil))
  "Loads config contained in **filepath**. If it's nil, searches for a default config file to load."
  (let ((yaml-config (if filepath
                         (yaml:parse filepath)
                         (search-and-load-config-file))))
    (when yaml-config (set-config yaml-config))))

(defun search-and-load-config-file ()
  "If a default config file exists, return its contents. Otherwise returns nil."
  (let ((config-path (merge-pathnames
                      *config-filename*
                      (directory-namestring
                       (nth 2 (multiple-value-list
                               (asdf:locate-system *asdf-system*)))))))
    ;; If a config file exists, return its contents
    (when (and config-path (probe-file config-path))
      (yaml:parse config-path))))

(defun set-config (config)
  "Sets configs parameters from **config**, a parsed yaml file. Always returns t to indicate that a new config was set."
  (set-single-config *docstrings-as-markdown?* "docstrings-as-markdown" config)
  t)



