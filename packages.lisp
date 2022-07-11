;;;; package.lisp

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
   #:draw-rectangle
   #:draw-square
   #:draw-node
   #:draw-grid
   #:draw-long-path
   #:with-random-crop
   ;; Utilities
   #:symb
   #:capture-stdout
   #:dohash
   #:list-to-set
   #:set-to-list
   #:nshuffle
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
