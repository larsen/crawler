(in-package :crawler)

(defclass dungeon-room ()
  ((x1 :reader x1
       :initarg :x1)
   (x2 :reader x2
       :initform 0)
   (y1 :reader y1
       :initarg :y1)
   (y2 :reader y2
       :initform 0)
   (width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)
   (region-id :accessor region-id
              :initform nil)))

(defmethod initialize-instance :after ((o dungeon-room) &key)
  (with-slots (x1 x2 y1 y2 width height) o
    (setf x2 (+ x1 width)
          y2 (+ y1 height))))

(defun calculate-room-count (density)
  (with-slots (width height room-min-max) *dungeon*
    (let* ((smallest-area (* (expt (first room-min-max) 2)))
           (largest-area (* (expt (second room-min-max) 2)))
           (average-area (/ (abs (- largest-area smallest-area)) 2))
           (possible-rooms (/ (* width height) average-area)))
      (floor (* possible-rooms density)))))

(defmethod generate-room-size ()
  (flet ((random-size (min max)
           (when (evenp min) (incf min))
           (+ min (* 2 (random (floor (+ 2 (- max min)) 2))))))
    (with-slots (room-min-max) *dungeon*
      (let* ((w (apply #'random-size room-min-max))
             (h (apply #'random-size room-min-max)))
        (if (< (/ (min w h) (max w h)) (random 1.0))
            (generate-room-size)
            (values w h))))))

(defmethod generate-room-location (w h)
  (with-slots (width height) *dungeon*
    (let ((x (random (- width w)))
          (y (random (- height h))))
      (values (if (evenp x) (incf x) x)
              (if (evenp y) (incf y) y)))))

(defmethod add-to-dungeon ((room dungeon-room))
  (with-slots (x1 x2 y1 y2 region-id) room
    (with-slots (data rooms regions) *dungeon*
      (loop for x from x1 below x2
            do (loop for y from y1 below y2
                     for tile = (make-tile x y :terrain :room :region-id region-id)
                     do (setf (aref data x y) tile)
                        (push tile (tiles (gethash region-id regions)))))
      (push room rooms))))

(defmethod create-room ()
  (multiple-value-bind (w h) (generate-room-size)
    (multiple-value-bind (x y) (generate-room-location w h)
      (let ((room (make-instance 'dungeon-room :x1 x :y1 y :w w :h h)))
        (with-slots (current-region regions) *dungeon*
          (with-slots (region-id) room
            (unless (intersectsp room)
              (setf region-id (incf current-region)
                    (gethash region-id regions) (make-instance 'region :id region-id))
              (add-to-dungeon room))))))))

(defmethod intersectsp ((new-room dungeon-room))
  (loop for room in (rooms *dungeon*)
        do (when (and (<= (x1 new-room) (x2 room))
                      (>= (x2 new-room) (x1 room))
                      (<= (y1 new-room) (y2 room))
                      (>= (y2 new-room) (y1 room)))
             (return t))))
