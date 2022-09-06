(in-package #:org.numbra.cl-tikz)

(defun latex-command (command &key mand-args opt-args)
  (format t "\\~A" (normalise-string command))
  (when opt-args (format-options t opt-args :mandatory nil :newline nil))
  (when mand-args (format-options t mand-args :mandatory t :newline nil))
  (format t "~%"))

(defmacro with-env ((env &key options) &body body)
  (let ((varenv (normalise-string env)))
    `(progn
       (format t "~&\\begin{~A}" ,varenv)
       (format-options t ,options)
       ,@body
       (format t "~&\\end{~A}" ,varenv))))

(defmacro with-tikz-command ((command &key options) &body body)
  (let ((varcommand (normalise-string command)))
    `(progn
       (format t "\\~A" ,varcommand)
       (format-options t ,options :newline nil)
       ,@body
       (format t ";~%"))))

(defun preamble (&key (documentclass :standalone) packages)
  "Write the preabme of the file.
PACKAGES is a list of cons cells, of the form
(PACKAGE-NAME . OPT-ARGS)
where PACKAGE-NAME is either a keyword or a list of one element
where OPT-ARGS is a list formatted as in `format-options'"
  (format t "%%%CODE AUTO GENERATED BY THE CL-TIKZ PACKAGE~%~%")
  (latex-command :documentclass  :mand-args documentclass)
  ;; Default packages
  (dolist (package '((:xcolor . (svgnames))
                     (:tikz . nil)
                     (:inputenc . (utf8))
                     (:pgfmath . nil)))
    (latex-command :usepackage
                   :mand-args (car package)
                   :opt-args (cdr package)))
  ;; Optional packages
  (dolist (package packages)
    (latex-command :usepackage
                   :mand-args (car package)
                   :opt-args (cdr package)))
  ;; Tikz libraries
  (latex-command :usetikzlibrary
                 :mand-args '(arrows
                              shapes
                              decorations.markings
                              decorations.pathmorphing)))

(defmacro with-preamble-to-file ((filename &key (documentclass :standalone) packages)
                                 (&rest keys &key &allow-other-keys)
                                 &body body)
  (with-gensyms (stream)
    `(with-open-file (,stream
                      ,filename
                      :direction :output
                      :if-exists :rename
                      :if-does-not-exist :create
                      ,@keys)
       (let ((*standard-output* ,stream))
         (preamble :packages ,packages :documentclass ,documentclass)
         (with-env (document)
             ,@body)
         (format t "~2%~@{~A~%~}"
                 "%%% Local Variables:"
                 "%%% mode: latex"
                 "%%% TeX-master: t"
                 "%%% End:")
         nil))))
