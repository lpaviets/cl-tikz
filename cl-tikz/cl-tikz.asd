;;;; cl-tikz.asd

(asdf:defsystem #:cl-tikz
  :description "Describe cl-tikz here"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "point")
               (:file "transformations")
               (:file "options")
               (:file "helpers")
               (:file "arrows")
               (:file "shapes")
               (:file "cl-tikz")))
