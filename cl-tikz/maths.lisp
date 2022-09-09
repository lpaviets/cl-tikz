(in-package #:org.numbra.cl-tikz/math)

(defstruct point
  x
  y)

(declaim (inline point))
(defun point (x y)
  (make-point :x x :y y))

(defmacro with-point ((x y) point &body body)
  `(with-accessors ((,x point-x)
                    (,y point-y))
       ,point
     ,@body))

(defun point-ensure (&rest args)
  "A DWIM convert-to-point function.
If ARGS is ...
- A POINT P -> return P.
- A cons cell (X . Y) -> return (POINT X Y)
- A list (X Y) -> return (POINT X Y)
- Two arguments, X and Y -> return (POINT X Y)"
  (if (= (length args) 1)
      (let ((point (car args)))
       (etypecase point
         (point point)
         (cons (if (listp (cdr point))
                   (point (car point) (cadr point))
                   (point (car point) (cdr point))))))
      (point (first args) (second args))))

(defun point-neighbours (point)
  "Return the four adjacent points to POINT, in order left, down, right
and up when compared to it, as a list"
  (with-point (x y) point
    (let ((left (point (1- x) y))
          (down (point x (1- y)))
          (right (point (1+ x) y))
          (up (point x (1+ y))))
      (list left down right up))))

(defun point-all-neighbours (point)
  "Return the eight adjacent points to POINT, in order left, down-left,
down, down-right, right, up-right, up, up-left when compared to it, as
a list"
  (with-point (x y) point
    (let ((left (point (1- x) y))
          (down-left (point (1- x) (1- y)))
          (down (point x (1- y)))
          (down-right (point (1+ x) (1- y)))
          (right (point (1+ x) y))
          (up-right (point (1+ x) (1+ y)))
          (up (point x (1+ y)))
          (up-left (point (1- x) (1+ y))))
      (list left down-left down down-right right up-right up up-left))))

(defvar *float-approx-digits* 2)

(defun point+ (pt-a pt-b)
  (make-point :x (+ (point-x pt-a) (point-x pt-b))
              :y (+ (point-y pt-a) (point-y pt-b))))

(defun point- (pt-a pt-b)
  (make-point :x (- (point-x pt-a) (point-x pt-b))
              :y (- (point-y pt-a) (point-y pt-b))))

(defun point*n (pt n)
  (make-point :x (* (point-x pt) n)
              :y (* (point-y pt) n)))

(defun point*pt (pt-a pt-b)
  (make-point :x (* (point-x pt-a) (point-x pt-b))
              :y (* (point-y pt-a) (point-y pt-b))))

(defun point-exp (pt n)
  (make-point :x (expt (point-x pt) n)
              :y (expt (point-y pt) n)))

(defun point-rotate (point angle &optional (center (point 0 0)))
  "Return the point obtained by rotating POINT by *ROTATION-ANGLE* radians
counterclockwise around *ROTATION-CENTER*"
  (let* ((diff (point- point center))
         (dx (point-x diff))
         (dy (point-y diff))
         (cos-angle (cos angle))
         (sin-angle (sin angle)))
    (point+ center
            (make-point :x (- (* cos-angle dx)
                              (* sin-angle dy))
                        :y (+ (* sin-angle dx)
                              (* cos-angle dy))))))

(defun line-equation-from (pt-a pt-b)
  "Return a list (A B C) such that the line given by the equation
AX+BY+C=0 passes through PT-A and PT-B"
  (with-point (xa ya) pt-a
    (with-point (xb yb) pt-b
      (let ((dx (- xb xa))
            (dy (- yb ya)))
        (if (zerop dx)
            (list 1 0 (- xa))
            (let* ((a (/ dy dx))
                   (c (- ya (* a xa))))
              (list a -1 c)))))))

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


(defun point-to-tikz (point)
  "Return a string formatted as \"(x, y)\" if POINT is #S(POINT :X X :Y Y).
The coordinates as printed as floating points, with *FLOAT-APPROX-DIGITS*
digits after the decimal point"
  (format nil "(~,vF, ~,vF)"
          *float-approx-digits*
          (point-x point)
          *float-approx-digits*
          (point-y point)))

(defun point-absolute-point (x y)
  (apply-transformations (point x y)))

(defun point-str (x y)
  "Format the point #S(POINT :X X :Y Y) as a string, after
having rotated it according to *ROTATION-CENTER* and *ROTATION-ANGLE*
See `point-to-tikz' for details on the output string"
  (point-to-tikz (point-absolute-point x y)))
