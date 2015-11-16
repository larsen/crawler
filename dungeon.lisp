(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((w :reader w
      :initarg :w)
   (h :reader h
      :initarg :h)
   (tile-size :reader tile-size
              :initarg :tile-size
              :initform 10)
   (rooms :accessor rooms
          :initform nil)
   (room-min-max :reader room-min-max
                 :initform '(5 13))
   (current-region :accessor current-region
                   :initform 0)
   (data :accessor data
         :initarg :data)))

(defun make-dungeon (&key w h tile-size (max-tries 1000) (density 0.75))
  (let ((data (make-array `(,w ,h))))
    (setf *dungeon* (make-instance 'dungeon :w w :h h :tile-size tile-size :data data))
    (create-walls)
    (create-rooms max-tries density)
    (create-corridors)
    (create-connectors)))

(defun calculate-room-count (density)
  (with-slots (w h room-min-max) *dungeon*
    (let* ((smallest-area (* (expt (first room-min-max) 2)))
           (largest-area (* (expt (second room-min-max) 2)))
           (average-area (/ (abs (- largest-area smallest-area)) 2))
           (possible-rooms (/ (* w h) average-area)))
      (floor (* possible-rooms density)))))

(defun create-walls ()
  (loop with (w h) = (array-dimensions (data *dungeon*))
        for x below w
        do (loop for y below h
                 do (setf (aref (data *dungeon*) x y) (make-tile)))))

(defun create-rooms (max-tries density)
  (loop with rooms = (calculate-room-count density)
        with tries = 0
        until (or (= (length (rooms *dungeon*)) rooms)
                  (>= tries max-tries))
        do (create-room *dungeon*)
           (incf tries)))

(defmethod create-corridors ()
  (with-slots (data) *dungeon*
    (loop for x from 1 below (1- (array-dimension data 0))
          do (loop for y from 1 below (1- (array-dimension data 1))
                   do (when (and (eq (terrain (aref data x y)) :wall)
                                 (eq (terrain (aref data (1- x) (1- y))) :wall)
                                 (eq (terrain (aref data x (1- y))) :wall)
                                 (eq (terrain (aref data (1+ x) (1- y))) :wall)
                                 (eq (terrain (aref data (1- x) y)) :wall)
                                 (eq (terrain (aref data (1+ x) y)) :wall)
                                 (eq (terrain (aref data (1- x) (1+ y))) :wall)
                                 (eq (terrain (aref data x (1+ y))) :wall)
                                 (eq (terrain (aref data (1+ x) (1+ y))) :wall))
                        (carve x y))))))
