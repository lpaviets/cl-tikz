(in-package #:org.numbra.cl-tikz/math)

;;;; Geometric transformations
;;;; Transformations are used via WITH-* macros. Each of those macros will
;;;; add a new transformation to be applied to all the points before *drawing*
;;;; them (user facing interface; in fact, the transformations are applied in
;;;; the POINT-ABSOLUTE-POINT function)
;;;; Nested transformations are applied sequentially. This means that you do not
;;;; need to be aware of the surrounding context to draw, and you can always assume
;;;; that the origin is the point (0, 0) and draw as usual.

(defvar *transformations* nil
  "List of geometric transformations to apply to each point.
Each element is a list (TRANSFO . ARGS) where transfo is a valid key for
*TRANSFORMATION-FUNCTIONS*, and ARGS are argument to the associated function.")

(defvar *transformation-functions*
  '(:rotate point-rotate
    :shift point+
    :mirror point-mirror)
  "PLIST of geometric transformations.
Each WITH-<TRANSFO> macro is supposed to be associated with a keyword,
the key in this PLIST, and a function TRANSFO, the associated value.
TRANSFO is a function whose lambda-list has the form
(POINT ARG1 ... ARGN)
It should return the value of POINT after being transformed by TRANSFO with
some extra parameters ARG1 ... ARGN.")

(defmacro with-transformation ((keyword &rest args) &body body)
  `(let ((*transformations* (cons (list ,keyword ,@args) *transformations*)))
     ,@body))

(defmacro with-rotation (angle (&optional center-x center-y) &body body)
  (let ((new-center (if (or center-x center-y)
                        `(point ,center-x ,center-y)
                        '(point 0 0))))
    `(with-transformation (:rotate ,angle ,new-center)
       ,@body)))

(defmacro with-reset-transformations (&body body)
  `(let ((*transformations* nil))
     ,@body))

(defmacro with-shift ((x y) &body body)
  `(with-transformation (:shift (point ,x ,y))
     ,@body))

(defun point-mirror (point pt-a pt-b)
  "Mirror of point POINT relative to the line that passes by PT-A and PT-B"
  (destructuring-bind (a b c) (line-equation-from pt-a pt-b)
    (with-point (x y) point
      (let ((coeff (* -2 (/ (+ (* a x)
                               (* b y)
                               c)
                            (+ (* a a)
                               (* b b))))))
        (point (+ x (* a coeff)) (+ y (* b coeff)))))))

(defmacro with-mirror ((pt-a pt-b) &body body)
  `(with-transformation (:mirror ,pt-a ,pt-b)
     ,@body))

(defun get-transformation-function (transfo)
  (getf *transformation-functions* transfo))

(defun apply-transformations (point)
  (loop :with new-point = point
        :for (transfo-type . transfo-args) :in *transformations*
        :for transfo-function = (get-transformation-function transfo-type)
        :do (assert transfo-function
                    nil
                    "No geometric transformation is associated to ~A" transfo-type)
        :do (setf new-point (apply transfo-function new-point transfo-args))
        :finally (return new-point)))

(defun point-absolute-point (x y)
  (apply-transformations (point x y)))

(defun point-str (x y)
  "Format the point #S(POINT :X X :Y Y) as a string, after
having rotated it according to *ROTATION-CENTER* and *ROTATION-ANGLE*
See `point-to-tikz' for details on the output string"
  (point-to-tikz (point-absolute-point x y)))
