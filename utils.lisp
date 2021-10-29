(in-package #:cl-tikz)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun to-lowercase-string (designator)
  (declare (type (or string symbol)))
  (string-downcase (string designator)))
