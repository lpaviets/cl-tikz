;;;; cl-tikz.asd

(asdf:defsystem #:cl-tikz
  :description "Describe cl-tikz here"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "helpers")
               (:file "paths")
               (:file "draw-tiles")
               (:file "cl-tikz")))
