(in-package #:org.numbra.cl-tikz-tilings)

(defclass topology ()
  ((content :initarg :content
            :reader content
            :documentation "The content of ")
   (bounds :initarg :bounds
           :reader bounds
           :type function
           :documentation "Function of one argument, a POINT. Should return T if POINT is in the
bounds of CONTENT, NIL otherwise. This function along with the
GET-TILE-FUN slot are used to changed the actual topology of the
tiling.")
   (get-tile-fun :initarg :tile-fun
                 :reader get-tile-fun
                 :type function
                 :documentation "Function of two arguments, a TILING and a POINT. Can assume that
POINT is in the bounds of TILING, and should return the tile located
at position POINT in TILING, or NIL if no such tile exists."))
  (:default-initargs
   :content (error "A content is required to define a tiling")
   :bounds (error "A bounding function is required to define a tiling")))
