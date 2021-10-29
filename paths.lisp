(in-package #:cl-tikz)

(defun draw-rectangle (xmin ymin xmax ymax &key (ostream t) options)
  (with-tikz-command (draw :ostream ostream :options options)
    (format ostream "(~a, ~a) rectangle (~a, ~a)" xmin ymin xmax ymax)))

(defun draw-square (xmin ymin &key (size 1) (ostream t) options)
  (let ((xmax (+ xmin size))
        (ymax (+ ymin size)))
    (draw-rectangle xmin ymin xmax ymax :ostream ostream :options options)))

(defun draw-grid (xmin ymin xmax ymax &key (step 1) ostream options)
  (with-tikz-command (draw :ostream ostream :options options)
    (format ostream "(~a, ~a) grid[step=~a] (~a, ~a)" xmin ymin step xmax ymax)))


(defun draw-long-path (xstart ystart &key (ostream t) options path)
  (with-tikz-command (draw :ostream ostream :options options)
    (loop :initially (format ostream "(~a, ~a)" xstart ystart)
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
                 (format ostream " -- (~a)" (to-lowercase-string val))
                 (format ostream " --+ (~a, ~a)" dx dy)))))
