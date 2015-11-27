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
   (doors :accessor doors
          :initform nil)
   (door-rate :reader door-rate
              :initarg :door-rate)
   (windiness :reader windiness
              :initarg :windiness)
   (connectors :accessor connectors
               :initform (make-hash-table))
   (tile-map :accessor tile-map
             :initarg :tile-map)
   (generator :accessor generator
              :initform nil)
   (seed :accessor seed
         :initform (get-universal-time))))

(defun make-dungeon (&key w h
                       (tile-size 10)
                       (room-density 0.1)
                       (windiness 0)
                       (door-rate 0.2)
                       (seed nil))
  (let ((w (if (evenp w) (1+ w) w))
        (h (if (evenp h) (1+ h) h)))
    (setf *dungeon* (make-instance 'dungeon
                                   :w w
                                   :h h
                                   :tile-size tile-size
                                   :tile-map (make-array `(,w ,h))
                                   :room-density room-density
                                   :windiness windiness
                                   :door-rate door-rate)))
  (init-generator seed)
  (create-walls)
  (create-rooms)
  (create-corridors)
  (create-connectors)
  (combine-dungeon)
  (remove-dead-ends))

(defun init-generator (seed)
  (when seed
    (setf (seed *dungeon*) seed))
  (setf (generator *dungeon*) (make-random-number-generator (seed *dungeon*))))

(defun create-walls ()
  (with-slots (width height tile-map) *dungeon*
    (loop for x below width
          do (loop for y below height
                   do (setf (aref tile-map x y) (make-tile x y))))))

(defun create-rooms ()
  (with-slots (room-density rooms) *dungeon*
    (loop with max-rooms = (calculate-room-count room-density)
          with tries = 0
          until (or (= (length rooms) max-rooms)
                    (>= tries 1000))
          do (create-room)
             (incf tries))))

(defmethod create-corridors ()
  (on-tile-map #'carvablep #'walkablep #'carve))

(defun create-connectors ()
  (on-tile-map #'possible-connector-p #'region-id #'add-connector))

(defun combine-dungeon ()
  (with-slots (generator regions) *dungeon*
    (loop with region-id = (random-element generator (hash-table-keys regions))
          while (connectors (gethash region-id regions))
          for door = (random-connector region-id)
          do (merge-region region-id door))))

(defun remove-dead-ends ()
  (with-slots (dead-ends-p) *dungeon*
    (loop while dead-ends-p
          do (setf dead-ends-p nil)
             (on-tile-map #'dead-end-p #'walkablep #'make-wall))))

(defun test ()
  (with-slots (width height tile-map) *dungeon*
    (make-array (* width height) :displaced-to tile-map)))
