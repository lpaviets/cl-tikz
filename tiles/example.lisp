(in-package #:cl-tikz/tiles)

(defun gen-example-tiling (tileset m n)
  (let ((name (format nil "./examples/test-~A-~Dx~D.tex" (tileset-name tileset) m n))
        (tiling (make-tiling-grid tileset m n)))
    (with-preamble-to-file (name) ()
      (with-env (tikzpicture)
        (with-random-crop (0.5 0.5 (- m 0.8) (- n 0.8))
          (draw-tiling (solve-naive tiling :random t)))))))

(defun gen-rao-jeandel (m n)
  (gen-example-tiling *rao-jeandel* m n))

(defun gen-kari-culik (m n)
  (gen-example-tiling *kari-culik* m n))
