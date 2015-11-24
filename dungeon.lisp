(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)
   (tile-size :reader tile-size
              :initarg :tile-size)
   (rooms :accessor rooms
          :initform nil)
   (regions :accessor regions
            :initform (make-hash-table))
   (room-min-max :reader room-min-max
                 :initform '(3 11))
   (current-region :accessor current-region
                   :initform 0)
   (dead-ends-p :accessor dead-ends-p
                :initform t)
   (room-density :reader room-density
                 :initarg :room-density)
   (door-rate :reader door-rate
              :initarg :door-rate)
   (data :accessor data
         :initarg :data)))

(defun make-dungeon (&key w h
                       (tile-size 10)
                       (room-density 0.75)
                       (door-rate 0.2))
  (let ((w (if (evenp w) (1+ w) w))
        (h (if (evenp h) (1+ h) h)))
    (setf *dungeon* (make-instance 'dungeon
                                   :w w
                                   :h h
                                   :tile-size tile-size
                                   :data (make-array `(,w ,h))
                                   :room-density room-density
                                   :door-rate door-rate)))
  (create-walls)
  (create-rooms)
  (create-corridors)
  (create-connectors)
  (combine-dungeon)
  (remove-dead-ends))

(defun create-walls ()
  (with-slots (width height data) *dungeon*
    (loop for x below width
          do (loop for y below height
                   do (setf (aref data x y) (make-tile x y))))))

(defun create-rooms ()
  (with-slots (room-density rooms) *dungeon*
    (loop with max-room = (calculate-room-count room-density)
          with tries = 0
          until (or (= (length rooms) max-room)
                    (>= tries 1000))
          do (create-room)
             (incf tries))))

(defmethod create-corridors ()
  (on-tile-map #'carvablep #'terrain #'carve))

(defun create-connectors ()
  (on-tile-map #'possible-connector-p #'region-id #'add-connector))

(defun combine-dungeon ()
  (with-slots (regions) *dungeon*
    (let* ((region-count (length (hash-table-keys regions)))
           (region (gethash (1+ (random region-count)) regions)))
      (loop while (connectors region)
            for door = (random-connector (id region))
            do (merge-region (id region) door)))))

(defun remove-dead-ends ()
  (with-slots (dead-ends-p) *dungeon*
    (loop while dead-ends-p
          do (setf dead-ends-p nil)
             (on-tile-map #'dead-end-p #'terrain #'make-wall))))
