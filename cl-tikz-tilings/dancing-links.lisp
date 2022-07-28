(in-package #:org.numbra.cl-tikz-tilings)

;;; Dancing Links X algorithm
;;; From https://github.com/desmondcheongzx/dancing-links

;; Data structures

(defclass data ()
  ((up :initarg :up :initform nil :accessor up)
   (down :initarg :down :initform nil :accessor down)
   (left :initarg :left :initform nil :accessor left)
   (right :initarg :right :initform nil :accessor right)
   (column :initarg :column :initform nil :accessor column)))

(defclass header-data (data)
  ((name :initarg :name :initform nil :accessor name)
   (size :initarg :size :initform 0 :accessor size)))

(defmethod initialize-instance :after ((x header-data) &key (columns nil))
  (setf (left x) x)
  (setf (column x) x)
  (unless (null columns)
    (loop
      :for prev = x :then (right prev)
      :for c :in columns
      :do (let ((new-data (make-instance 'header-data :name c :right x)))
            (setf (up new-data) new-data)
            (setf (down new-data) new-data)
            (setf (right prev) new-data)
            (setf (left new-data) prev)
            (setf (left x) new-data)))))

;; Utilities

(defmacro initialize-matrix (columns matrix place)
  `(progn (setf ,place (make-instance 'header-data
                                      :name 'header
                                      :columns ,columns))
          (loop :for row :in ,matrix :do (add-row row ,place))))

(defmacro traverse-matrix (var start dir &rest body)
  `(loop
     :for ,var = (,dir ,start) :then (,dir ,var)
     :until (equal ,var ,start)
     :do (progn ,@body)))

(defun print-solution (solution)
  (loop
    :initially (format t "Selected lines:~%")
    :for i :in (reverse solution)
    :do (if (eq i :split)
            (format t "~&")
           (format t "~a/" i))))

(defun solution-to-list (solution)
  (loop :with row = nil
        :for i :in solution
        :if (eq i :split)
          :collect row
          :and :do (setf row nil)
        :else
          :do (push i row)))

;; Dancing Links Algorithm

(defun choose-column (matrix)
  (let ((s most-positive-fixnum)
        (chosen-column matrix))
    (traverse-matrix column matrix right
                     (when (< (size column) s)
                       (setf s (size column))
                       (setf chosen-column column)))
    chosen-column))

(defun cover-column (column)
  (setf (right (left column)) (right column))
  (setf (left (right column)) (left column)) ;dance column out
  (traverse-matrix row column down ;dance row out
                   (traverse-matrix i row right
                                    (decf (size column))
                                    (setf (down (up i)) (down i))
                                    (setf (up (down i)) (up i)))))

(defun uncover-column (column)
  (setf (right (left column)) column)
  (setf (left (right column)) column) ;dance column in
  (traverse-matrix row column up ;dance row in
                   (traverse-matrix i row left
                                    (incf (size column))
                                    (setf (down (up i)) i)
                                    (setf (up (down i)) i))))

(defmacro add-row (row matrix)
  "Adds a row of data objects to matrix"
  `(let ((leftmost nil))
     (loop
       :for i :in ,row
       :for c = (first-column ,matrix) :then (right c)
       :do (when (= i 1)
            (let ((new-data (insert-data c)))
              (if (null leftmost)
                  (progn
                    (setf leftmost new-data)
                    (setf (left leftmost) leftmost)
                    (setf (right leftmost) leftmost))
                  (progn
                    (setf (right new-data) leftmost)
                    (setf (left new-data) (left leftmost))
                    (setf (right (left leftmost)) new-data)
                    (setf (left leftmost) new-data))))))))


(defun first-column (matrix) (right matrix))

(defun insert-data (column)
  "Inserts a data object into a given column. Returns the new data object."
  (let ((new-data (make-instance 'data
                                 :up (up column)
                                 :down column
                                 :column column)))
    (setf (down (up column)) new-data)
    (setf (up column) new-data)
    (incf (size column)) ;increase the size of the column
    new-data))

(defun %solve-dancing-links (matrix solution)
  (let ((column (choose-column matrix))
        answer)
    (if (equal column matrix)
        solution
        (unless (zerop (size column))
          (push :split solution)
          (push (name column) solution)
          (cover-column column)
          (traverse-matrix row column down
                           (traverse-matrix i row right
                                            (cover-column (column i))
                                            (push (name (column i)) solution))
                           (setf answer (%solve-dancing-links matrix solution))
                           (traverse-matrix i row left
                                            (uncover-column (column i))
                                            (pop solution)))
          (uncover-column column)
          (pop solution)
          (pop solution)
          answer))))

(defun solve-dancing-links (rows-list &optional columns-names)
  (let ((num-cols (length (car rows-list))))
    (let ((columns-names (or columns-names
                             (loop :for i :below num-cols :collect i))))
      (let (matrix)
        (initialize-matrix columns-names rows-list matrix)
        (solution-to-list (%solve-dancing-links matrix nil))))))
