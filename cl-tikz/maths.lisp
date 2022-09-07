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

(defvar *rotation-angle* 0d0)
(defvar *rotation-center* (point 0 0))
(defvar *float-approx-digits* 2)
(defvar *origin* (point 0 0))

(defmacro with-rotation (angle (&optional center-x center-y) &body body)
  (let ((new-center (if (or center-x center-y)
                        `(point ,center-x ,center-y)
                        '*rotation-center*)))
    `(let ((*rotation-angle* ,angle)
           (*rotation-center* ,new-center))
       ,@body)))

(defmacro with-reset-rotation (&body body)
  `(let ((*rotation-angle* 0)
         (*rotation-center* (point 0 0)))
     ,@body))

(defmacro with-relative-rotation (angle (&optional center-x center-y) &body body)
  `(let ((*rotation-angle* (+ *rotation-angle* ,angle))
         (*rotation-center* ,(if (or center-x center-y)
                                 `(point+ *rotation-center*
                                             (point ,center-x ,center-y))
                                 '*rotation-center*)))
     ,@body))

(defmacro with-shift ((x y) &body body)
  `(let ((*origin* (point+ *origin* (point ,x ,y))))
     ,@body))

(defmacro with-reset-shift (&body body)
  `(let ((*origin* (point 0 0)))
     ,@body))

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

(defun point-rotate (point)
  "Return the point obtained by rotating POINT by *ROTATION-ANGLE* radians
counterclockwise around *ROTATION-CENTER*"
  (let* ((diff (point- point *rotation-center*))
         (dx (point-x diff))
         (dy (point-y diff))
         (cos-angle (cos *rotation-angle*))
         (sin-angle (sin *rotation-angle*)))
    (point+ *rotation-center*
            (make-point :x (- (* cos-angle dx)
                              (* sin-angle dy))
                        :y (+ (* sin-angle dx)
                              (* cos-angle dy))))))

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
  (point+ *origin* (point-rotate (point x y))))

(defun point-str (x y)
  "Format the point #S(POINT :X X :Y Y) as a string, after
having rotated it according to *ROTATION-CENTER* and *ROTATION-ANGLE*
See `point-to-tikz' for details on the output string"
  (point-to-tikz (point-absolute-point x y)))
