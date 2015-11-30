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
   (region-id :accessor region-id)))

(defmethod initialize-instance :after ((o dungeon-room) &key)
  (with-slots (x1 x2 y1 y2 width height) o
    (setf x2 (+ x1 width)
          y2 (+ y1 height))))

(defun calculate-room-count (density)
  (with-slots (width height) *dungeon*
    (let* ((smallest-area (* (expt (attr 'room-size-min) 2)))
           (largest-area (* (expt (attr 'room-size-max) 2)))
           (average-area (/ (abs (- largest-area smallest-area)) 2))
           (possible-rooms (/ (* width height) average-area)))
      (floor (* possible-rooms density)))))

(defun generate-room-size ()
  (let ((w (rng 'odd-range :min (attr 'room-size-min) :max (attr 'room-size-max)))
        (h (rng 'odd-range :min (attr 'room-size-min) :max (attr 'room-size-max))))
    (if (< (/ (min w h) (max w h)) (rng 'range-i))
        (generate-room-size)
        (values w h))))

(defun generate-room-location (width height)
  (let ((x (rng 'int :max (- (width *dungeon*) width 1)))
        (y (rng 'int :max (- (height *dungeon*) height 1))))
    (values (if (evenp x) (incf x) x)
            (if (evenp y) (incf y) y))))

(defun add-to-dungeon (room)
  (with-slots (x1 x2 y1 y2 region-id) room
    (with-slots (tile-map rooms regions current-region) *dungeon*
      (setf region-id (incf current-region)
            (gethash region-id regions) (make-instance 'region :id region-id :roomp t))
      (loop for x from x1 below x2
            do (loop for y from y1 below y2
                     for tile = (make-tile x y :walkablep t :region-id region-id)
                     do (setf (aref tile-map x y) tile)
                        (push tile (tiles (gethash region-id regions)))))
      (push room rooms))))

(defun create-room ()
  (multiple-value-bind (w h) (generate-room-size)
    (multiple-value-bind (x y) (generate-room-location w h)
      (let ((room (make-instance 'dungeon-room :x1 x :y1 y :w w :h h)))
        (unless (intersectsp room)
          (add-to-dungeon room))))))

(defun intersectsp (new-room)
  (loop for room in (rooms *dungeon*)
        do (when (and (<= (x1 new-room) (x2 room))
                      (>= (x2 new-room) (x1 room))
                      (<= (y1 new-room) (y2 room))
                      (>= (y2 new-room) (y1 room)))
             (return room))))
