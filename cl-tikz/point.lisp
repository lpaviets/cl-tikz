(in-package #:org.numbra.cl-tikz/math)

(defstruct point
  (x 0 :type number)
  (y 0 :type number)
  (z nil))

(declaim (inline point))
(defun point (x y &optional z)
  (make-point :x x :y y :z z))

(defmacro with-point ((x y &optional z) point &body body)
  `(with-accessors ((,x point-x)
                    (,y point-y)
                    ,@(when z (list (list z 'point-z))))
       ,point
     ,@body))

(defun ensure-2d-point (point)
  (if (point-z point)
      point
      (error "Point ~S is not a 2D point" point)))

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
  (with-point (x y) (ensure-2d-point point)
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

(defun point= (pt-a pt-b)
  (and (= (point-x pt-a) (point-x pt-b))
       (= (point-y pt-a) (point-y pt-b))
       (and (point-z pt-a)
            (point-z pt-b)
            (= (point-z pt-a) (point-z pt-b)))))

(defun point+ (pt-a pt-b)
  (make-point :x (+ (point-x pt-a) (point-x pt-b))
              :y (+ (point-y pt-a) (point-y pt-b))
              :z (and (point-z pt-a)
                      (point-z pt-b)
                      (+ (point-z pt-a) (point-z pt-b)))))

(defun point- (pt-a pt-b)
  (make-point :x (- (point-x pt-a) (point-x pt-b))
              :y (- (point-y pt-a) (point-y pt-b))
              :z (and (point-z pt-a)
                      (point-z pt-b)
                      (- (point-z pt-a) (point-z pt-b)))))

(defun point*n (pt n)
  (make-point :x (* (point-x pt) n)
              :y (* (point-y pt) n)
              :z (and (point-z pt)
                      (* (point-z pt) n))))

(defun point*pt (pt-a pt-b)
  (make-point :x (* (point-x pt-a) (point-x pt-b))
              :y (* (point-y pt-a) (point-y pt-b))
              :z (and (point-z pt-a)
                      (point-z pt-b)
                      (* (point-z pt-a) (point-z pt-b)))))

(defun point-exp (pt n)
  (make-point :x (expt (point-x pt) n)
              :y (expt (point-y pt) n)
              :z (and (point-z pt)
                      (expt (point-z pt) n))))

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
                              (* cos-angle dy))
                        :z (point-z point)))))

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

(defvar *3d-enabled* t)
(defmacro with-3d-p (3d-enabled &body body)
  `(let ((*3d-enabled* ,3d-enabled))
     ,@body))

(defun point-to-tikz (point)
  "Return a string formatted as \"(x, y)\" if POINT is #S(POINT :X X :Y Y).
The coordinates as printed as floating points, with *FLOAT-APPROX-DIGITS*
digits after the decimal point"
  (with-point (x y z) point
    (format nil "(~,vF, ~,vF~:[~;, ~,vF~])"
            *float-approx-digits* x
            *float-approx-digits* y
            (and *3d-enabled* z)
            *float-approx-digits* z)))
