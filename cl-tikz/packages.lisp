;;;; packages.lisp

(defpackage #:cl-tikz/math
  (:use #:cl)
  (:export
   ;; Parameters
   #:*float-approx-digits*
   ;; Point
   #:point
   #:point-x
   #:point-y
   #:with-point
   #:point-neighbours
   ;; Macros
   #:with-rotation
   #:with-reset-rotation
   #:with-relative-rotation
   #:with-shift
   #:with-reset-shift
   ;; Drawing
   #:point-str))

(defpackage #:cl-tikz
  (:use #:cl #:cl-tikz/math)
  (:export
   ;; Draw paths
   #:draw-line
   #:draw-rectangle
   #:draw-square
   #:draw-node
   #:draw-grid
   #:draw-long-path
   #:with-random-crop
   ;; Utilities
   #:xor
   #:symb
   #:capture-stdout
   #:with-gensyms
   #:dohash
   #:list-to-set
   #:set-to-list
   #:nshuffle
   #:rotate-sequence
   #:format-options
   #:latex-command
   #:with-env
   #:with-tikz-command
   #:with-preamble-to-file))
