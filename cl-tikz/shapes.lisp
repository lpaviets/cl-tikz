(in-package #:cl-tikz)

(defun draw-line (x1 y1 x2 y2 &key options)
  (with-tikz-command (draw :options options)
    (format t " ~A -- ~A" (point-str x1 y1) (point-str x2 y2))))

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
          :with x = xstart
          :and y = ystart
          :for (dir val) :on path :by #'cddr
          :do
             (ccase dir
               ((:l :left) (decf x val))
               ((:r :right) (incf x val))
               ((:u :up) (incf y val))
               ((:d :down) (decf y val))
               ((:c :cycle)
                (assert (null val) ()
                        ":cycle should be the last element of the path"))
               ((:n :node)))
             (case dir
               ((:n :node) (format t " -- (~A)" (to-lowercase-string val)))
               ((:c :cycle) (format t "-- cycle"))
               (t (format t " -- ~A" (point-str x y)))))))

(defmacro with-random-crop ((xmin ymin xmax ymax &key (step 2)) &body body)
  `(with-env (scope)
     (latex-command :pgfmathsetseed :mand-args (list 12345))
     (draw-rectangle ,xmin ,ymin ,xmax ,ymax
                     :options '((:decoration . ,(format nil "{random steps, segment length=~Amm}" step))
                                :decorate
                                :clip))
     ,@body))