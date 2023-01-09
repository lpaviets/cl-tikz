;;;; packages.lisp

(defpackage #:org.numbra.cl-tikz/math
  (:use #:cl)
  (:export
   ;; Parameters
   #:*float-approx-digits*
   ;; Point
   #:point
   #:point-ensure
   #:point-absolute-point
   #:point-x
   #:point-y
   #:point+
   #:point-
   #:point*n
   #:point*pt
   #:point-exp
   #:with-point
   #:point-neighbours
   #:point-all-neighbours
   ;; Macros
   #:with-rotation
   #:with-shift
   #:with-mirror
   #:with-reset-transformation
   ;; Drawing
   #:point-str))

(defpackage #:org.numbra.cl-tikz
  (:use #:cl #:org.numbra.cl-tikz/math)
  (:export
   ;; Draw paths
   #:defshape
   #:draw-square
   #:draw-node
   #:draw-edge
   #:draw-long-path
   #:with-random-crop
   ;; Other drawing utilities
   #:option-arrow-head-at
   ;; LaTeX/TikZ utilities
   #:format-options
   #:make-options
   #:latex-command
   #:with-env
   #:with-tikz-command
   #:with-preamble-to-file
   ;; Generic utilities
   #:xor
   #:symb
   #:capture-stdout
   #:with-gensyms
   #:dohash
   #:list-to-set
   #:set-to-list
   #:nshuffle
   #:rotate-sequence))
