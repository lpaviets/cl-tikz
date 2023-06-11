(in-package #:org.numbra.cl-tikz)

;;; TODO:
;;; - macro to define at the same time a %SHAPE and a DRAW-SHAPE function
;;; - add optional parameters to e.g. rectangle: "...rectangle[some opt] ..."
;;; - find a clean way to make it "available" to the user, without having separate
;;; macros for everything ?

;;; Kinda disappointing:
;;; - keyword arguments are handled in a shaky way
;;; - it generates a bunch of functions for no real reason, we could imagine
;;; something that "dispatches" on a symbol (e.g. (draw-shape <name> args)
;;; rather than (draw-<name> args)). COUNTERPOINT: becomes hard to give
;;; interactive arglist
;;;- Only able to produce tikz DRAW commands, no node, clip etc.


;; (defgeneric draw (thing &rest args))
;; (defgeneric fill (thing &rest args))
;; (defgeneric clip (thing &rest args))

(defmacro defshape (name
                    (&rest args)
                    (&rest key-args)
                    (&key (export t) (clip t))
                    &body body)
  (assert (symbolp name) () "~S has to be a symbol" name)
  (let ((%fun (symb "%" name))
        (draw-fun (symb 'draw- name))
        (fill-fun (symb 'fill- name))
        (clip-fun (symb 'with- name '-clip))
        (fun-args `(,@args ,@(when key-args
                               (list* '&key key-args))))
        (draw-fun-args `(,@args &key options ,@key-args)))
    (flet ((parse-key-args (kargs)
             (loop :for arg :in kargs
                   :if (atom arg)
                     :collect (intern (symbol-name arg)
                                      (find-package "KEYWORD"))
                     :and :collect arg
                   :else
                     :collect (intern (symbol-name (car arg))
                                      (find-package "KEYWORD"))
                     :and :collect (car arg))))
      `(progn
         ;; The basic drawing function
         (defun ,%fun ,fun-args
           ,@body)
         ;; The DRAW-<NAME> macro, simple wrapper around the previous
         ;; one
         (defun ,draw-fun ,draw-fun-args
           (with-tikz-command (:draw :options options)
             (,%fun ,@args ,@(parse-key-args key-args))))
         (defun ,fill-fun ,draw-fun-args
           (with-tikz-command (:fill :options options)
             (,%fun ,@args ,@(parse-key-args key-args))))
         ;; The WITH-<NAME>-CLIP macro, used to have a simpler way to
         ;; "crop" a drawing according to some predefined shape.
         ,(when clip
            `(defmacro ,clip-fun (,fun-args &body body)
               `(with-env (:scope)
                  (with-tikz-command (:clip)
                    (,',%fun ,,@args ,,@(parse-key-args key-args)))
                  ,@body)))
         ,(when export
            `(export '(,draw-fun ,(when clip clip-fun))
                     (find-package "ORG.NUMBRA.CL-TIKZ")))))))

(defshape line (x1 y1 x2 y2) ()
    (:clip nil)
  (format t " ~A -- ~A" (point-str x1 y1) (point-str x2 y2)))

(defshape rectangle (xmin ymin xmax ymax) () ()
  (format t " ~A rectangle ~A" (point-str xmin ymin) (point-str xmax ymax)))

(defshape grid (xmin ymin xmax ymax) ((step 1))
    (:clip nil)
  (format t " ~A grid[step=~D] ~A"
          (point-str xmin ymin)
          step
          (point-str xmax ymax)))

(defshape node (x y) (name label)
    (:clip nil)
  (format t " ~@[(~A)~] at ~A {~@[~A~]}" name (point-str x y) label))

(defun add-node (x y &key name label options)
  (with-tikz-command (:node :options options)
    (format t " ~@[(~A)~] at ~A {~@[~A~]}" name (point-str x y) label)))

(defun draw-square (xmin ymin &key (size 1) options)
  (let ((xmax (+ xmin size))
        (ymax (+ ymin size)))
    (draw-rectangle xmin ymin xmax ymax :options options)))

(defun draw-edge (node-1 node-2 style &key options)
  (with-tikz-command (:path :options style)
    (format t "(~A) edge~A (~A)"
            node-1
            (format-options nil options :newline nil)
            node-2)))

(defun draw-long-relative-path (xstart ystart path &key options)
  (with-tikz-command (:draw :options options)
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
               ((:n :node) (format t " -- (~A)" (normalise-string val)))
               ((:c :cycle) (format t "-- cycle"))
               (t (format t " -- ~A" (point-str x y)))))))

(defun draw-long-path (points &key cycle options)
  (with-tikz-command (draw :options options)
    (format t "~{~A~^ -- ~}~:[~; -- cycle~]"
            (mapcar (lambda (pt)
                      (let ((point (point-ensure pt)))
                        (with-point (x y) point
                          (point-str x y))))
                    points)
            cycle)))

(defmacro with-random-crop ((xmin ymin xmax ymax &key (step 2)) &body body)
  `(with-env (:scope)
     (latex-command :pgfmathsetseed :mand-args (list 12345))
     (draw-rectangle ,xmin ,ymin ,xmax ,ymax
                     :options
                     (make-options :decoration
                                   ,(format nil
                                            "{random steps, segment length=~Amm}"
                                            step)
                                   :decorate t
                                   :clip t))
     ,@body))
