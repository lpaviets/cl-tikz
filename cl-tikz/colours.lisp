(in-package #:org.numbra.cl-tikz)

(defclass colour-rgb ()
  ((red :accessor colour-red
        :initarg :red)
   (green :accessor colour-green
          :initarg :green)
   (blue :accessor colour-blue
         :initarg :blue)))

(defun colour-rgb (r g b)
  (make-instance 'colour-rgb :red r :green g :blue b))

(defun colour-format (colour)
  (format nil
          "{rgb:red,~D;green,~D;blue,~D}"
          (colour-red colour)
          (colour-green colour)
          (colour-blue colour)))

(defun colour-mix-names (&rest colours)
  "COLOURS is a list of pairs, (COLOUR INTENSITY)
Returns a string formatted using the LaTeX xcolor's package ! notation
For example, (COLOUR-MIX '(RED 50) '(\"LimeGreen\" 30)) returns the string
\"red!50!LimeGreen!30\"
Symbols are downcased, strings as used as is."
  (format nil "~:{~A!~D~}" (mapcar (lambda (colour)
                                     (list (normalise-string (first colour))
                                           (second colour)))
                                   colours)))

(defun colour-mix-colours (&rest colours)
  "COLOURS is a list of colours"
  (let ((count (length colours)))
    (colour-format
     (colour-rgb
      (/ (reduce #'+ colours :key 'colour-red) count)
      (/ (reduce #'+ colours :key 'colour-green) count)
      (/ (reduce #'+ colours :key 'colour-blue) count)))))
