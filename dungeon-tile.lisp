(in-package :crawler)

(defclass tile ()
  ((terrain :accessor terrain
            :initarg :terrain)
   (region :accessor region
           :initarg :region)
   (visitedp :accessor visitedp
             :initform nil)
   (connectorp :accessor connectorp
               :initform nil)))

(defun make-tile (&key (terrain :wall) region)
  (let ((tile (make-instance 'tile :terrain terrain :region region)))
    (when (eq (terrain tile) :room)
      (setf (visitedp tile) t))
    tile))

(defun create-connectors ()
  (with-slots (data) *dungeon*
    (loop for x from 1 below (1- (array-dimension data 0))
          do (loop for y from 1 below (1- (array-dimension data 1))
                   for tile = (aref data x y)
                   for n = (aref data x (1- y))
                   for s = (aref data x (1+ y))
                   for e = (aref data (1+ x) y)
                   for w = (aref data (1- x) y)
                   do (when (and (not (region tile))
                                 (not (connectorp tile))
                                 (or (and (not (eql (region n) (region s)))
                                          (region n) (region s))
                                     (and (not (eql (region e) (region w)))
                                          (region e) (region w))))
                        (setf (connectorp tile) t))))))
