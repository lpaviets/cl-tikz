(in-package #:cl-tikz)

(defun format-options (options &key ostream)
  (let ((res (mapcar (lambda (x)
                       (if (consp x)
                           (format nil "~a=~a"
                                   (to-lowercase-string (car x))
                                   (to-lowercase-string (cdr x)))
                           (format nil "~a" (to-lowercase-string x))))
                     options)))
    (format ostream "[~{~a~^, ~}]~%" res)))

(defmacro with-env ((env &key ostream options) &body body)
  (let ((varenv (to-lowercase-string env)))
    `(progn
       (format ,ostream "~&\\begin{~a}" ,varenv)
       (format-options ,options :ostream ,ostream)
       ,@body
       (format ,ostream "~&\\end{~a}" ,varenv))))

(defmacro with-tikz-command ((command &key ostream options) &body body)
  (let ((varcommand (to-lowercase-string command)))
   `(progn
      (format ,ostream "\\~a" ,varcommand)
      (format-options ,options :ostream ,ostream)
      ,@body
      (format ,ostream ";~%"))))
