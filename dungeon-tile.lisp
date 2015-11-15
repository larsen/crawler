(in-package :crawler)

(defclass tile ()
  ((terrain :accessor terrain
            :initarg :terrain)
   (region :accessor region
           :initarg :region)
   (visitedp :accessor visitedp
             :initform nil)))

(defun make-tile (&key (terrain :wall) (region 0))
  (let ((tile (make-instance 'tile :terrain terrain :region region)))
    (when (eq (terrain tile) :room)
      (setf (visitedp tile) t))
    tile))
