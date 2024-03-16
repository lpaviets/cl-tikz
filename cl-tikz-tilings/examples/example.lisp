(in-package #:org.numbra.cl-tikz-tilings)

(defparameter *examples-directory*
  #P"output-examples/")

(defun gen-example-tiling (tileset m n &key (solver 'solve-naive) (crop t) with-grid)
  (let* ((filename (merge-pathnames (format nil "test-~A-~Dx~D.tex"
                                            (tileset-name tileset)
                                            m n)
                                    *examples-directory*))
         (tiling (make-tiling-grid tileset m n))
         (solution (funcall solver tiling :random t)))
    (when solution
      (with-preamble-to-file (filename) ()
        (with-env (:tikzpicture)
          (if crop
              (with-random-crop (0.5 0.5 (- m 0.8) (- n 0.8))
                (draw-tiling solution :with-grid with-grid))
              (draw-tiling solution :with-grid with-grid)))))
    solution))

(defun gen-example-periodic (tileset m n &key (solver 'solve-naive) (crop t) with-grid)
  (let* ((filename (merge-pathnames (format nil "test-periodic-~A-~Dx~D.tex"
                                            (tileset-name tileset)
                                            m n)
                                    *examples-directory*))
         (tiling (make-tiling-torus tileset m n))
         (solution (funcall solver tiling :random t)))
    (when solution
      (with-preamble-to-file (filename) ()
        (with-env (:tikzpicture)
          (if crop
              (with-random-crop (0.5 0.5 (- m 0.8) (- n 0.8))
                (draw-tiling solution :with-grid with-grid))
              (draw-tiling solution :with-grid with-grid)))))
    solution))
