(in-package #:cl-tikz/tiles)

;;; Tile sets

(defclass tileset ()
  ((size :initarg :size
         :initform (error "Must specify a size for the tileset")
         :accessor tileset-size
         :type fixnum)
   (tiles :initarg :tiles
          :initform nil
          :accessor tileset-tiles
          :type array)
   (rules :initarg :rules
          :initform nil
          :accessor tileset-rules)
   (sides :initarg :sides
          :initform nil
          :accessor tileset-sides
          :type array)))

(defun make-tileset (size)
  (let ((tileset (make-instance 'tileset :size size)))
    (with-accessors ((tiles tileset-tiles)
                     (rules tileset-rules)
                     (sides tileset-sides))
        tileset
      (setf tiles (make-array size :initial-element nil)
            rules (make-rules size)
            sides (make-array size :initial-element nil)))
    tileset))

(defun tileset-add-tile-function (tileset id fun)
  (setf (aref (tileset-tiles tileset) id) fun))

(defun tileset-get-tile-function (tileset id)
  (aref (tileset-tiles tileset) id))

(defun tileset-add-tile-sides (tileset tile sides)
  (setf (aref (tileset-sides tileset) tile) sides))

(defmethod add-rules ((t-a tileset) (t-b tileset))
  (add-rules (tileset-rules t-a) (tileset-rules t-b)))

(defmethod add-rules ((tileset tileset) r)
  (add-rules (tileset-rules tileset) r))

(defun tileset-tile-side (tileset tile side)
  (let ((sides (tileset-sides tileset)))
    (case side
      (:left (nth 0 (aref sides tile)))
      (:down (nth 1 (aref sides tile)))
      (:right (nth 2 (aref sides tile)))
      (:up (nth 3 (aref sides tile))))))

(defun tileset-make-sides-table (tileset)
  (let ((size (tileset-size tileset))
        (all-sides-table (make-hash-table)))
    (dolist (side '(:left :down :right :up) all-sides-table)
      (setf (gethash side all-sides-table) (make-hash-table))
      (dotimes (id size)
        (let ((tile-side (tileset-tile-side tileset id side)))
          (push id (gethash tile-side (gethash side all-sides-table))))))))

(defun tileset-fill-side-rules-from-table (tileset table side)
  (let ((size (tileset-size tileset))
        (rules (funcall (rules-side-name side) (tileset-rules tileset)))
        (opp-side (ccase side
                    (:right :left)
                    (:up :down))))
    ;; Fill the rules-<side> table:
    ;; Go through the list of tiles whose <opposite>-side is the same as our SIDE
    (loop :with opp-side-table = (gethash opp-side table)
          :for tile :below size :do
            (loop :with tile-side = (tileset-tile-side tileset tile side)
                  :for j :below size
                  :do (setf (aref rules tile j)
                            (not (null (member j (gethash tile-side opp-side-table)))))))))

(defun tileset-make-rules-from-sides (tileset)
  (let ((sides-table (tileset-make-sides-table tileset)))
    (tileset-fill-side-rules-from-table tileset sides-table :right)
    (tileset-fill-side-rules-from-table tileset sides-table :up)))
