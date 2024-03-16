(in-package #:org.numbra.cl-tikz-tilings)

(defparameter *examples-directory*
  #P"output-examples/")

(defun gen-example-tiling (tileset m n &key (solver 'solve-naive) (crop t) (with-grid t) (preprocess nil))
  "Generate a random solution using TILESET for a grid of size MxN.

Ther resulting .tex file is stored in the directory `*examples-directory*'.

SOLVER can be changed to solve the problem with a different algorithm. This is a
function of two arguments, a TILING and a key RANDOM to determine if the
solution should be randomized or not.

CROP and WITH-GRID change the final .tex.

PREPROCESS is NIL or a function of one argument, a TILING, returning
another (possibly the same) TILING. It can be used to e.g. change the initial
value of the tiling."
  (let* ((filename (merge-pathnames (format nil "test-~A-~Dx~D.tex"
                                            (tileset-name tileset)
                                            m n)
                                    *examples-directory*))
         (%tiling (make-tiling-grid tileset m n))
         (tiling (if preprocess (funcall preprocess %tiling) %tiling))
         (solution (funcall solver tiling :random t)))
    (when solution
      (with-preamble-to-file (filename) ()
        (with-env (:tikzpicture)
          (if crop
              (with-random-crop (0.5 0.5 (- m 0.8) (- n 0.8))
                (draw-tiling solution :with-grid with-grid))
              (draw-tiling solution :with-grid with-grid)))))
    solution))

(defun gen-example-periodic (tileset m n &key (solver 'solve-naive) (crop t) (with-grid t))
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
