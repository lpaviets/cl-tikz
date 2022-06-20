(in-package #:cl-tikz)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun to-lowercase-string (designator)
  (typecase designator
    ((or string symbol) (string-downcase (string designator)))
    (t (princ-to-string designator))))

(defmacro capture-stdout (&body body)
  "Redirect all the things printed on stdout by BODY to a string, and
return this string once BODY terminates"
  (let ((string (gensym)))
    `(let ((,string (make-string-output-stream)))
       (let ((*standard-output* ,string))
         ,@body)
       (get-output-stream-string ,string))))
