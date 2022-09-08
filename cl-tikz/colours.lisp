(in-package #:org.numbra.cl-tikz)

(defun colour-rgb (r g b))

(defun colour-mix (&rest colours)
  "COLOURS is a list of lists of length 2, (COLOUR INTENSITY)
Returns a string formatted using the LaTeX xcolor's package ! notation
For example, (COLOUR-MIX '(RED 50) '(\"LimeGreen\" 30)) returns the string
\"red!50!LimeGreen!30\"
Symbols are downcased, strings as used as is."
  (format nil "~:{~A!~D~}" (mapcar (lambda (colour)
                               (list (normalise-string (first colour))
                                     (second colour)))
                             colours)))
