(in-package :crawler)

(defvar *dungeon* nil)
(defvar *room-density* 0.75)

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
                 :initform '(5 17))
   (data :accessor data
         :initarg :data)))

(defun make-dungeon (&key w h tile-size (max-tries 100000))
  (let ((data (make-array `(,w ,h))))
    (setf *dungeon* (make-instance 'dungeon :w w :h h :tile-size tile-size :data data))
    (carve 'square (data *dungeon*) (random w) (random h))
    (loop with rooms = (calculate-room-count)
          with tries = 0
          until (or (= (length (rooms *dungeon*)) rooms)
                    (>= tries max-tries))
          do (create-room *dungeon*)
             (incf tries))))

(defun calculate-room-count ()
  (with-slots (w h room-min-max) *dungeon*
    (let* ((smallest-area (* (expt (first room-min-max) 2)))
           (largest-area (* (expt (second room-min-max) 2)))
           (average-area (/ (abs (- largest-area smallest-area)) 2))
           (possible-rooms (/ (* w h) average-area)))
      (floor (* possible-rooms *room-density*)))))
