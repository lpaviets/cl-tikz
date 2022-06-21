(TeX-add-style-hook
 "test-output-kari-culik"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8")))
   (TeX-run-style-hooks
    "latex2e"
    "standalone"
    "standalone10"
    "tikz"
    "color"
    "inputenc"
    "pgfmath"))
 :latex)

