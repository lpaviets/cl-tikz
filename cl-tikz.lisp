;;;; cl-tikz.lisp

(in-package #:cl-tikz)

(defun to-lowercase-string (designator)
  (declare (type (or string symbol)))
  (string-downcase (string designator)))

(defun format-options (options &key ostream)
  (let ((res (mapcar (lambda (x)
                       (if (consp x)
                           (format nil "~a=~a"
                                   (to-lowercase-string (car x))
                                   (to-lowercase-string (cdr x)))
                           (format nil "~a" (to-lowercase-string x))))
                     options)))
    (format ostream "[~{~a~^, ~}]~%" res)))

(defmacro with-env ((env options &key ostream) &body body)
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

(defun draw-rectangle (xmin ymin xmax ymax &key ostream options)
  (with-tikz-command (draw :ostream ostream :options options)
    (format ostream "(~a, ~a) rectangle (~a, ~a)" xmin ymin xmax ymax)))

(defun draw-square (xmin ymin size &key ostream options)
  (let ((xmax (+ xmin size))
        (ymax (+ ymin size)))
    (draw-rectangle xmin ymin xmax ymax :ostream ostream :options options)))

(defun draw-grid (xmin ymin xmax ymax &key (step 1) ostream options)
  (with-tikz-command (draw :ostream ostream :options options)
    (format ostream "(~a, ~a) grid[step=~a] (~a, ~a)" xmin ymin step xmax ymax)))
