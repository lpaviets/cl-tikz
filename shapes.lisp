(in-package #:cl-tikz)

(defun draw-rectangle (xmin ymin xmax ymax &key options)
  (with-tikz-command (draw :options options)
    (format t " (~a, ~a) rectangle (~a, ~a)" xmin ymin xmax ymax)))

(defun draw-square (xmin ymin &key (size 1) options)
  (let ((xmax (+ xmin size))
        (ymax (+ ymin size)))
    (draw-rectangle xmin ymin xmax ymax :options options)))

(defun draw-grid (xmin ymin xmax ymax &key (step 1) options)
  (with-tikz-command (draw :options options)
    (format t " (~a, ~a) grid[step=~a] (~a, ~a)" xmin ymin step xmax ymax)))


(defun draw-long-path (xstart ystart path &key options)
  (with-tikz-command (draw :options options)
    (loop :initially (format t " (~a, ~a)" xstart ystart)
          :with dx = 0
          :with dy = 0
          :for (dir val) :on path :by #'cddr
          :do
             (ccase dir
               ((:l :left) (setf dx (- val)))
               ((:r :right (setf dx val)))
               ((:u :up) (setf dy val))
               ((:d :down) (setf dy (- val)))
               ((:n :node)))
             (if (member dir '(:n :node))
                 (format t " -- (~a)" (to-lowercase-string val))
                 (format t " --++ (~a, ~a)" dx dy)))))
