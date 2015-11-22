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
   (w :reader w
      :initarg :w)
   (h :reader h
      :initarg :h)
   (region-id :accessor region-id
              :initform nil)))

(defmethod initialize-instance :after ((o dungeon-room) &key)
  (with-slots (x1 x2 y1 y2 w h) o
    (setf x2 (+ x1 w)
          y2 (+ y1 h))))

(defmethod generate-room-size ((dungeon dungeon))
  (flet ((random-size (min max)
           (when (evenp min) (incf min))
           (+ min (* 2 (random (floor (+ 2 (- max min)) 2))))))
    (with-slots (room-min-max) dungeon
      (let* ((w (apply #'random-size room-min-max))
             (h (apply #'random-size room-min-max)))
        (if (< (/ (min w h) (max w h)) (random 1.0))
            (generate-room-size dungeon)
            (values w h))))))

(defmethod generate-room-location ((dungeon dungeon) width height)
  (with-slots (w h) dungeon
    (let ((x (random (- w width)))
          (y (random (- h height))))
      (values (if (evenp x) (incf x) x)
              (if (evenp y) (incf y) y)))))

(defmethod add-to-dungeon ((room dungeon-room) (dungeon dungeon))
  (with-slots (x1 x2 y1 y2 region-id) room
    (with-slots (data rooms) dungeon
      (loop for x from x1 below x2
            do (loop for y from y1 below y2
                     for tile = (make-tile x y :terrain :room :region-id region-id)
                     do (setf (aref data x y) tile)
                        (push tile (tiles (gethash region-id (regions *dungeon*))))))
      (push room rooms))))

(defmethod create-room ((dungeon dungeon))
  (multiple-value-bind (w h) (generate-room-size dungeon)
    (multiple-value-bind (x y) (generate-room-location dungeon w h)
      (let ((room (make-instance 'dungeon-room :x1 x :y1 y :w w :h h)))
        (with-slots (current-region regions) dungeon
          (with-slots (region-id) room
            (unless (intersectsp room dungeon)
              (setf region-id (incf current-region)
                    (gethash region-id regions) (make-instance 'region :id region-id))
              (add-to-dungeon room dungeon))))))))

(defmethod intersectsp ((new-room dungeon-room) (dungeon dungeon))
  (loop for room in (rooms dungeon)
        do (when (and (<= (x1 new-room) (x2 room))
                      (>= (x2 new-room) (x1 room))
                      (<= (y1 new-room) (y2 room))
                      (>= (y2 new-room) (y1 room)))
             (return t))))
