(in-package #:cl-tikz/tiles)

;;; Adjacency rules

(defstruct (rules
            (:constructor make-rules (size)))
  "A simple structure to store adjacency rules for a tiling with SIZE tiles
In each array SIDE, the cell in position (I, J) is T if the tile I can
have a tile J on its side SIDE"
  (size (error "Must specify a size") :type fixnum :read-only t)
  (right (make-array (list size size) :initial-element nil))
  (up (make-array (list size size) :initial-element nil)))

(defun rules-side-name (side)
  "Convert SIDE to its corresponding accessor for the structure RULES"
  (ccase side
    ((right :right rules-right) 'rules-right)
    ((up :up rules-up) 'rules-up)))

(defun rules-opposite-side-name (side)
  (ccase side
    ((left :left rules-left) 'rules-right)
    ((down :down rules-down) 'rules-up)))

(defmacro dorules ((i j side cell) rules &body body)
  (let ((varsize (gensym)))
      `(let ((,varsize (rules-size ,rules)))
         (loop :for ,side :in '(rules-right rules-up) :do
          (loop :for ,i :below ,varsize :do
            (loop :for ,j :below ,varsize :do
              (let ((,cell (aref (funcall ,side ,rules) ,i ,j)))
                ,@body)))))))

;;; Adding rules

(defgeneric add-rules (rules-a rules-b)
  (:documentation "Add the rules of RULES-B to RULES-A
If no information about two tiles I, J has ever been given, rules always
assuming that they cannot be placed next to each other.
However, as soon as information about either of I or J is given, the rules
are updated as if all the relevant information had been given.
This means that e.g. forbidding the pattern IJ implicitly allows all the other
patterns IK where K is not J.
Said differently, when filling the rules incrementally, each row is only ever
modified once."))

(defmethod add-rules ((rules-a rules) (rules-b rules))
  "Add the constraints of RULES-B to rules-A
We assume that the same ID refer to the same tile in both sets of rules.
Moreover, no consistency check is made, we only add the forbidden patterns
of RULES-B to RULES-A "
  (dorules (i j side cell) rules-b
    (unless cell
      (let ((rules-side-a (funcall side rules-a)))
        (setf (aref rules-side-a i j) nil)))))

(defun add-rules-plist (rules tile plist)
  "Helper function for `ADD-RULES' specialized on types RULES and LIST
SIDE is a valid side keyword, e.g. :LEFT or :UP"
  (let ((forbidp (getf plist :forbid)))
    (loop :for side :in '(:right :up)
          :for rules-side = (rules-side-name side)
          :for new-rules = (getf plist side)
          :if new-rules
            :do(dotimes (j (rules-size rules))
                 (if forbidp
                     (setf (aref (funcall rules-side rules) tile j)
                           (if (member j new-rules) nil t))
                     (setf (aref (funcall rules-side rules) tile j)
                           (if (member j new-rules) t nil)))))
    (loop :for side :in '(:left :down)
          :for rules-opp-side = (rules-opposite-side-name side)
          :for new-rules = (getf plist side)
          :if new-rules
            :do (dotimes (j (rules-size rules))
                  (if forbidp
                      (setf (aref (funcall rules-opp-side rules) j tile)
                            (if (member j new-rules) nil t))
                      (setf (aref (funcall rules-opp-side rules) j tile)
                            (if (member j new-rules) t nil)))))))

(defmethod add-rules ((rules-a rules) (rules-b list))
  "RULEs-B is supposed to be a list of the following form:
Each element represents rules for a single tile, and it is a cons cell,
whose car is a tile ID and whose cdr is a plist where:
  - keywords are :LEFT, :RIGHT, :UP, :DOWN or :FORBID
  -values are lists of tile IDs.
If :FORBID it T, then the elements are interpreted as constraints, i.e. as
forbidden patterns. Otherwise, they are seen as valid adjacency rules."
  (dolist (r rules-b)
    (add-rules-plist rules-a (car r) (cdr r))))
