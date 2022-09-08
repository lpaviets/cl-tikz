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

(defvar *transformations* nil "List of geometric transformations to apply to each point.
Each element is either (:SHIFT POINT) or (:ROTATE ANGLE CENTER)
where POINT and CENTER are points, ANGLE an angle in radian.")

(defmacro with-rotation (angle (&optional center-x center-y) &body body)
  (let ((new-center (if (or center-x center-y)
                        `(point ,center-x ,center-y)
                        '(point 0 0))))
    `(let ((*transformations* (cons (list :rotate ,angle ,new-center)
                                    *transformations*)))
       ,@body)))

(defmacro with-reset-transformations (&body body)
  `(let ((*transformations* nil))
     ,@body))

(defmacro with-shift ((x y) &body body)
  `(let ((*transformations* (cons (list :shift (point ,x ,y)) *transformations*)))
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

(defun apply-transformations (point)
  (loop :with new-point = point
        :for transfo :in *transformations*
        :for transfo-type = (car transfo)
        :do (ecase transfo-type
              (:rotate (setf new-point (point-rotate new-point
                                                     (second transfo)
                                                     (third transfo))))
              (:shift (setf new-point (point+ new-point (second transfo)))))
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
