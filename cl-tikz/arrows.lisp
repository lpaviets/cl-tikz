(in-package #:org.numbra.cl-tikz)

(defun option-arrow-head-at (pos reversed &key (style 'stealth))
  (format nil "decoration={markings, mark=at position ~F with {\\arrow~:[~;reversed~]{~A}}}, postaction={decorate}"
          pos
          reversed
          (normalise-string style)))
