(in-package #:org.numbra.cl-tikz)

;;; This file contains utilities to format, create and handle
;;; list of options for LaTeX and TikZ commands commands
;;; Options are internally simply stored as lists
;;; Each element is either a cons cell, a symbol, or a string
;;; A symbol or a string designates its lowercase name, and a
;;; cons cell (OPT . PARAMS) represents the option "opt=params"

;;; As a special, a string or a symbol will be considered as
;;; a list of one option.

(defun option-to-latex (option)
  (when option
    (if (consp option)
        (format nil "~A=~A"
                (normalise-string (car option))
                (normalise-string (cdr option)))
        (normalise-string option))))

(defun format-options (destination options &key (newline t) mandatory)
  (let ((res (if (and options ;; don't format NIL
                      (or (symbolp options)
                          (stringp options))) ;; Be liberal in what we accept
                 (list (normalise-string options))
                 (mapcar 'option-to-latex options)))
        (op (if mandatory "{" "["))
        (cl (if mandatory "}" "]")))
    (format destination "~A~{~A~^, ~}~A~:[~;~%~]" op res cl newline)))

(defun make-options (&rest options)
  "Return a list of options.
OPTIONS is a plist, whose keys represent the option name and whose
values correspond to its parameters.
As a special case, a value that is EQ to T is considered to take
no parameter, e.g. (MAKE-OPTIONS :COLOR 'RED :DECORATE T) returns
((:COLOR . RED) DECORATE), and will be formatted as
[color=red, decorate]"
  (loop :for (opt val) :on options :by #'cddr
        :if (eq val t)
          :collect opt
        :else
          :collect (cons opt val)))
