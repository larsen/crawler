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
           :initarg :h)))

(defmethod initialize-instance :after ((o dungeon-room) &key)
  (with-slots (x1 x2 y1 y2 width height) o
    (setf x2 (+ x1 width)
          y2 (+ y1 height))))

(defun calculate-room-count (density)
  "Calculate an estimated number of rooms to generate for the specified density, based on the
minimum and maximum room sizes"
  (with-slots (width height) *dungeon*
    (let* ((smallest-area (* (expt (attr 'room-size-min) 2)))
           (largest-area (* (expt (attr 'room-size-max) 2)))
           (average-area (/ (abs (- largest-area smallest-area)) 2))
           (possible-rooms (/ (* width height) average-area)))
      (floor (* possible-rooms density)))))

(defun generate-room-size ()
  "Generate a random room size within the minimum and maximum sizes."
  (let ((w (rng 'odd-range :min (attr 'room-size-min) :max (attr 'room-size-max)))
        (h (rng 'odd-range :min (attr 'room-size-min) :max (attr 'room-size-max))))
    (if (< (/ (min w h) (max w h)) (rng 'range-i))
        (generate-room-size)
        (values w h))))

(defun generate-room-location (width height)
  "Generate a random location in the dungeon for a room."
  (let ((x (rng 'int :max (- (width *dungeon*) width 1)))
        (y (rng 'int :max (- (height *dungeon*) height 1))))
    (values (if (evenp x) (incf x) x)
            (if (evenp y) (incf y) y))))

(defun add-to-dungeon (room)
  "Add the given room to the dungeon."
  (with-slots (x1 x2 y1 y2) room
    (loop with region-id = (make-region)
          for x from x1 below x2
          do (loop for y from y1 below y2
                   for tile = (tile x y)
                   do (setf (walkablep tile) t
                            (region-id tile) region-id)))
    (push room (rooms *dungeon*))))

(defun create-room ()
  "Create a room with a random size and location, placing it in the dungeon."
  (multiple-value-bind (w h) (generate-room-size)
    (multiple-value-bind (x y) (generate-room-location w h)
      (let ((room (make-instance 'dungeon-room :x1 x :y1 y :w w :h h)))
        (unless (intersectsp room)
          (add-to-dungeon room))))))

(defun intersectsp (new-room)
  "Check whether a room overlaps another room."
  (loop for room in (rooms *dungeon*)
        do (when (and (<= (x1 new-room) (x2 room))
                      (>= (x2 new-room) (x1 room))
                      (<= (y1 new-room) (y2 room))
                      (>= (y2 new-room) (y1 room)))
             (return room))))
