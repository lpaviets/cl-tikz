;;;; package.lisp

(defpackage #:cl-tikz/math
  (:use #:cl)
  (:export
   ;; Point
   #:point
   #:make-point
   #:point-x
   #:point-y
   #:add-point
   #:sub-point
   #:rotate-point
   #:point-to-tikz
   #:point-str
   ;; Macros
   #:with-rotation
   #:with-reset-rotation
   #:with-relative-rotation
   #:with-shift
   #:with-reset-shift))

(defpackage #:cl-tikz
  (:use #:cl #:cl-tikz/math)
  (:export
   ;; Draw paths
   #:draw-rectangle
   #:draw-square
   #:draw-node
   #:draw-grid
   #:draw-long-path
   #:with-random-crop
   ;; Utilities
   #:symb
   #:capture-stdout
   #:format-options
   #:latex-command
   #:with-env
   #:with-tikz-command
   #:with-preamble-to-file))

(defpackage #:cl-tikz/tiles
  (:use #:cl #:cl-tikz #:cl-tikz/math)
  (:export
   ;; Tiles
   #:deftile
   #:draw-tiling
   #:def-wang-tileset
   ;; Solver
   #:solve-naive
   #:solve-dancing-links))
