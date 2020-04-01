;;;; ========================================================================================== ;;;;
;;;; string.lisp                                                                                ;;;;
;;;; ========================================================================================== ;;;;

(in-package #:litterae)

(defun empty? (string)
  "Return t if `string' is empty or nil, return nil otherwise."
  (zerop (length string)))

(defun not-empty? (string)
  "Return t if `string' is not empty, return nil otherwise."
  (not (empty? string)))

(defun concat (&rest strings)
  "Concatenate `strings' into a single string."
  (apply #'concatenate 'string strings))

(defun concat-as-string (&rest args)
  "Concatenate `args' as strings into a single string. If any of the
   arguments is not a string, it is converted with `write-to-string'."
  (let ((concat-strs
          (reduce
           #'(lambda (arg1 arg2)
               (concat
                (when arg1
                  (if (stringp arg1)
                      arg1
                      (write-to-string arg1)))
                (when arg2
                  (if (stringp arg2)
                      arg2
                      (write-to-string arg2)))))
           args)))
    (or concat-strs "")))

(defun concat-as-lines (&rest args)
  "Concatenate `args' separating each other with a newline character."
  (let ((concatenated-string
          (reduce
           (lambda (arg1 arg2)
             (if (or (null arg1)
                     (null arg2)
                     (string-equal arg1 "~%")
                     (string-equal arg2 "~%"))
                 (concat-as-string arg1 arg2)
                 (concat-as-string arg1 "~%" arg2)))
           args)))
    (if (not-empty? concatenated-string)
        (concat-as-string concatenated-string "~%") ; Add linebreak at the end of the final string
        "")))

(defun surround-string (str1 str2 &rest rest)
  "Surround strings from `rest' with `str1' and `str2'."
  (concat str1 (reduce #'concat rest) str2))

(defun replace-string (old new string)
  "Return a string with `old' replaced by `new' in `string'."
  (let ((old-location (search old string)))
    (if old-location
        (concat-as-string (subseq string 0 old-location)
                          new
                          (subseq string (+ old-location (length old))))
        (concat-as-string string new)))) ; If old is not found, concat new and string
