(in-package #:cl-tikz)

(defstruct point
  x
  y)

(defvar *rotation-angle* 0d0)
(defvar *rotation-center* (make-point :x 0 :y 0))
(defvar *float-approx-digits* 2)

(defmacro with-rotation (angle (&optional center-x center-y) &body body)
  (let ((new-center (if (or center-x center-y)
                        `(make-point :x ,center-x :y ,center-y)
                        '*rotation-center*)))
    `(let ((*rotation-angle* ,angle)
           (*rotation-center* ,new-center))
       ,@body)))

(defmacro with-reset-rotation (&body body)
  `(let ((*rotation-angle* 0)
         (*rotation-center* (make-point :x 0 :y 0)))
     ,@body))

(defmacro with-relative-rotation (angle (&optional center-x center-y) &body body)
  `(let ((*rotation-angle* (+ *rotation-angle* ,angle))
         (*rotation-center* ,(if (or center-x center-y)
                                 `(add-point *rotation-center*
                                             (make-point :x ,center-x :y ,center-y))
                                 '*rotation-center*)))
     ,@body))

(defun add-point (pt-a pt-b)
  (make-point :x (+ (point-x pt-a) (point-x pt-b))
              :y (+ (point-y pt-a) (point-y pt-b))))

(defun sub-point (pt-a pt-b)
  (make-point :x (- (point-x pt-a) (point-x pt-b))
              :y (- (point-y pt-a) (point-y pt-b))))

(defun rotate-point (point)
  "Return the point obtained by rotating POINT by *ROTATION-ANGLE* radians
counterclockwise around *ROTATION-CENTER*"
  (let* ((diff (sub-point *rotation-center* point))
         (dx (point-x diff))
         (dy (point-y diff))
         (cos-angle (cos *rotation-angle*))
         (sin-angle (sin *rotation-angle*)))
    (add-point *rotation-center*
               (make-point :x (+ (* cos-angle dx)
                                 (* sin-angle dy))
                           :y (- (* cos-angle dy)
                                 (* sin-angle dx))))))

(defun point-to-tikz (point)
  "Return a string formatted as \"(x, y)\" if POINT is #S(POINT :X X :Y Y).
The coordinates as printed as floating points, with *FLOAT-APPROX-DIGITS*
digits after the decimal point"
  (format nil "(~,vF, ~,vF)"
          *float-approx-digits*
          (point-x point)
          *float-approx-digits*
          (point-y point)))

(defun point-str (x y)
  "Format the point #S(POINT :X X :Y Y) as a string, after
having rotated it according to *ROTATION-CENTER* and *ROTATION-ANGLE*
See `point-to-tikz' for details on the output string"
  (point-to-tikz (rotate-point (make-point :x x :y y))))
