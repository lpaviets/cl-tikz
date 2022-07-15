(in-package #:cl-tikz-tilings)

(defparameter *examples-directory*
  #P"examples/")

(defun gen-example-tiling (tileset m n &key (solver 'solve-naive))
  (let* ((filename (merge-pathnames (format nil "test-~A-~Dx~D.tex" (tileset-name tileset) m n)
                                   *examples-directory*))
         (tiling (make-tiling-grid tileset m n))
         (solution (funcall solver tiling :random t)))
    (with-preamble-to-file (filename) ()
      (with-env (tikzpicture)
        (with-random-crop (0.5 0.5 (- m 0.8) (- n 0.8))
          (draw-tiling solution))))
    solution))
