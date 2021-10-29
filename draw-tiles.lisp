(in-package #:cl-tikz)

(defmacro deftile ((set id)
                   (&key background other-args)
                   &body body)
  "Creates a function MAKE-TILE-<SET>-<ID> which takes as arguments:
- X, Y (mandatory arguments): the position of the tile.
- SIZE (keyword argument): The size of the tile. Default to 1.
- OSTREAM (keyword argument): an output stream
Note that this makes the deftile macro anaphoric.
Indeed, in BODY, X, Y, SIZE and OSTREAM will be captured by the generated
function, while the programmer never explicitly gave them as arguments.
- OTHER-ARGS: this should be a list of keys

Other arguments to this macro are:
- SET: a hash-table. The keys are of the same type as ID, and the values
are the functions MAKE-TILE-SET-ID
- ID: a hashable unique identifier for the tile
- BODY: the body of the generated function
- BACKGROUND: If not nil, the tile will first be filled with this colour
on the background
- OTHER-ARGS: this should be a list, each element being a symbol or a
pair (symbol default-value). It will be inserted as-is in the parameter
list of the generated function"
  (let ((fun-name (symb 'make-tile- set #\- id))
        (fun-args (append '(&key (size 1) ostream) other-args)))
    `(progn
       (defun ,fun-name (x y ,@fun-args)
         ,(format nil
                  "Function creating the tile ~a of the tileset ~a.
See `deftile' for more information"
                  id
                  set)
         (declare (ignorable size))
         ,(when background
            `(with-tikz-command (fill :ostream ostream :options '((fill . ,background)))
               (format ostream "(~a, ~a) --+ (~a, ~a)" x y size size)))
         ,@body
         (format ostream "~&"))
       (setf (gethash ,id ,set) #',fun-name))))


(defun draw-tiling (solution set &key ostream other-args)
  "Draw the tiling corresponding to SOLUTION with the tileset SET.
SOLUTION is a 2D-array, each cell of which is an ID belonging to SET"
  (destructuring-bind (n m) (array-dimensions solution)
    (loop :for i :below n :do
      (loop :for j :below m :do
        (let* ((tile (aref solution i j))
               (fun-tile (gethash tile set)))
          (apply fun-tile i j :ostream ostream other-args))))
    (draw-grid 0 0 m n
               :ostream ostream
               :options '(thin))))
