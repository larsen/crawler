(in-package :crawler)

(defclass game-world ()
  ())

(defun get-tile (x y)
  (let* ((id (aref *world* x y))
        (type (cdr (assoc id *tile-types*))))
    (prototype type)))

(defun make-world (size)
  (setf *world* (make-array size))
  )
