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
  (with-attrs (width height) :dungeon
    (with-attrs (room-size-min room-size-max) :mine
      (setf room-size-min (clamp room-size-min 3 99)
            room-size-max (clamp room-size-max (+ room-size-min 2) 101))
      (let* ((smallest-area (* (expt room-size-min 2)))
             (largest-area (* (expt room-size-max 2)))
             (average-area (/ (abs (- largest-area smallest-area)) 2))
             (possible-rooms (/ (* width height) average-area)))
        (floor (* possible-rooms (clamp density 0.1 1)))))))

(defun generate-room-size ()
  (with-attrs (room-size-min room-size-max) :mine
    (let ((w (rng 'range-odd :min room-size-min :max room-size-max))
          (h (rng 'range-odd :min room-size-min :max room-size-max)))
      (if (< (/ (min w h) (max w h)) (rng 'range-inc))
          (generate-room-size)
          (values w h)))))

(defun add-to-dungeon (room)
  (with-slots (x1 x2 y1 y2) room
    (loop :with region-id = (make-region)
          :for x :from x1 :below x2
          :do (loop :for y :from y1 :below y2
                    :for tile = (tile x y)
                    :do (setf (walkablep tile) t
                              (region-id tile) region-id)
                        (add-feature tile :room)))
    (push room (rooms *dungeon*))))

(defun create-room ()
  (multiple-value-bind (w h) (generate-room-size)
    (with-attrs (width height) :dungeon
      (let* ((x (rng 'range-odd :max (- width w)))
             (y (rng 'range-odd :max (- height h)))
             (room (make-instance 'dungeon-room :x1 x :y1 y :w w :h h)))
        (unless (intersectsp room)
          (add-to-dungeon room))))))

(defun intersectsp (new-room)
  (loop :for room in (rooms *dungeon*)
        :do (when (and (<= (x1 new-room) (x2 room))
                       (>= (x2 new-room) (x1 room))
                       (<= (y1 new-room) (y2 room))
                       (>= (y2 new-room) (y1 room)))
              (return room))))

(defun roomp (tile)
  (featuresp tile '(:room)))
