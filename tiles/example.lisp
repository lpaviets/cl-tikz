(in-package #:cl-tikz/tiles)

(defun random-test-file ()
  (with-preamble-to-file ("./test-output.tex") ()
    (with-env (tikzpicture)
      (let ((test-set (make-tileset 3)))
        (deftile (test-set 0 :test) ()
          (draw-square 0 0 :options '(fill red)))
        (deftile (test-set 1 :test) (:background :blue)
          (draw-square 0 0))
        (deftile (test-set 2 :test) ()
          (draw-wang-tile 0 0 :red :blue :green :black))
        (let ((solution (make-array '(3 3)
                                    :initial-contents (loop :repeat 3
                                                            :collect
                                                            (loop :repeat 3
                                                                  :collect
                                                                  (random 3))))))
          (draw-tiling solution test-set))))))


(defun gen-example-tiling (tileset filename m n)
  (let ((name (format nil "./examples/test-~A.tex" filename)))
    (with-preamble-to-file (name) ()
      (with-env (tikzpicture)
        (with-random-crop (0.5 0.5 (- n 0.8) (- m 0.8))
          (draw-tiling (solver-naive tileset
                                     (list m n)
                                     t)
                       tileset))))))

(defun gen-rao-jeandel (m n)
  (gen-example-tiling *rao-jeandel* 'rao-jeandel m n))

(defun gen-kari-culik (m n)
  (gen-example-tiling *kari-culik* 'kari-culik m n))
