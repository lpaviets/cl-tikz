(in-package #:cl-tikz)

(defun format-options (options &key (ostream t) (newline t) mandatory)
  (let ((res (if (keywordp options)
                 (list (to-lowercase-string options))
                 (mapcar (lambda (x)
                           (if (consp x)
                               (format nil "~a=~a"
                                       (to-lowercase-string (car x))
                                       (to-lowercase-string (cdr x)))
                               (format nil "~a" (to-lowercase-string x))))
                         options)))
        (op (if mandatory "{" "["))
        (cl (if mandatory "}" "]")))
    (format ostream "~a~{~a~^, ~}~a~:[~;~%~]" op res cl newline)))

(defun latex-command (command &key ostream mand-args opt-args)
  (format ostream "\\~a" (to-lowercase-string command))
  (when opt-args (format-options opt-args :ostream ostream :mandatory nil :newline nil))
  (when mand-args (format-options mand-args :ostream ostream :mandatory t :newline nil))
  (format ostream "~%"))

(defmacro with-env ((env &key (ostream t) options) &body body)
  (let ((varenv (to-lowercase-string env)))
    `(progn
       (format ,ostream "~&\\begin{~a}" ,varenv)
       (format-options ,options :ostream ,ostream)
       ,@body
       (format ,ostream "~&\\end{~a}" ,varenv))))

(defmacro with-tikz-command ((command &key (ostream t) options) &body body)
  (let ((varcommand (to-lowercase-string command)))
   `(progn
      (format ,ostream "\\~a" ,varcommand)
      (format-options ,options :ostream ,ostream :newline nil)
      ,@body
      (format ,ostream ";~%"))))

(defun preamble (&key (ostream t) (documentclass :standalone) packages)
  "Write the preabme of the file.
PACKAGES is a list of cons cells, of the form
(PACKAGE-NAME . OPT-ARGS)
where PACKAGE-NAME is either a keyword or a list of one element
where OPT-ARGS is a list formatted as in `format-options'"
  (format ostream "%%%CODE AUTO GENERATED BY THE CL-TIKZ PACKAGE~%~%")
  (latex-command :documentclass :ostream ostream :mand-args documentclass)
  ;; Default packages
  (dolist (package '((:tikz . nil)
                     (:color . nil)
                     (:inputenc . (utf8))))
    (latex-command :usepackage
                   :ostream ostream
                   :mand-args (car package)
                   :opt-args (cdr package)))
  ;; Optional packages
  (dolist (package packages)
    (latex-command :usepackage
                   :ostream ostream
                   :mand-args (car package)
                   :opt-args (cdr package)))
  ;; Tikz libraries
  (latex-command :usetikzlibrary
                 :ostream ostream
                 :mand-args '(arrows shapes decorations.markings)))

(defmacro with-preamble-to-file ((filename &key packages)
                                 (&rest keys &key &allow-other-keys)
                                 &body body)
  (let ((stream (gensym)))
    `(with-open-file (,stream
                      ,filename
                      :direction :output
                      :if-exists :rename
                      :if-does-not-exist :create
                      ,@keys)
       (let ((*standard-output* ,stream))
         (preamble :packages ,packages)
         (with-env (document)
             ,@body)))))
