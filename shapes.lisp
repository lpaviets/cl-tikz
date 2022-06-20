(in-package #:cl-tikz)

(defun draw-rectangle (xmin ymin xmax ymax &key options)
  (with-tikz-command (draw :options options)
    (format t " ~A rectangle ~A" (point-str xmin ymin) (point-str xmax ymax))))

(defun draw-square (xmin ymin &key (size 1) options)
  (let ((xmax (+ xmin size))
        (ymax (+ ymin size)))
    (draw-rectangle xmin ymin xmax ymax :options options)))

(defun draw-grid (xmin ymin xmax ymax &key (step 1) options)
  (with-tikz-command (draw :options options)
    (format t " ~A grid[step=~D] ~A" (point-str xmin ymin) step (point-str xmax ymax))))

(defun draw-node (x y &key name label options)
  (with-tikz-command (node :options options)
    (format t " ~@[(~A)~] at ~A {~@[~A~]}" name (point-str x y) label)))

(defun draw-long-path (xstart ystart path &key options)
  (with-tikz-command (draw :options options)
    (loop :initially (format t " ~A" (point-str xstart ystart))
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
                 (format t " -- (~A)" (to-lowercase-string val))
                 (format t " --++ ~A" (point-str dx dy))))))

(defmacro with-random-crop ((xmin ymin xmax ymax &key (step 2)) &body body)
  `(with-env (scope)
     (latex-command :pgfmathsetseed :mand-args (list 12345))
     (draw-rectangle ,xmin ,ymin ,xmax ,ymax
                     :options '((:decoration . ,(format nil "{random steps, segment length=~Amm}" step))
                                :decorate
                                :clip))
     ,@body))
