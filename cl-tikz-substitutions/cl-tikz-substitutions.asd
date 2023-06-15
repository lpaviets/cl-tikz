;;;; cl-tikz-substitutions.asd

(asdf:defsystem #:cl-tikz-substitutions
  :description "Describe cl-tikz here"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-tikz)
  :components ((:file "packages")
               (:file "utils")
               (:file "planar-graph")
               (:file "geometric")
               (:file "examples")
               (:file "group-subst")))
