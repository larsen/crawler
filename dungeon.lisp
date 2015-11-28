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
   (current-region :accessor current-region
                   :initform 0)
   (dead-ends-p :accessor dead-ends-p
                :initform t)
   (doors :accessor doors
          :initform nil)
   (connectors :accessor connectors
               :initform (make-hash-table))
   (tile-map :accessor tile-map
             :initarg :tile-map)))

(defun make-dungeon (&key w h (tile-size 10)
                       room-size-min
                       room-size-max
                       room-density
                       door-rate
                       windiness
                       seed)
  (let ((w (if (evenp w) (1+ w) w))
        (h (if (evenp h) (1+ h) h)))
    (setf *dungeon* (make-instance 'dungeon
                                   :w w
                                   :h h
                                   :tile-size tile-size
                                   :tile-map (make-array `(,w ,h)))))
  (make-generator :seed seed
                  :room-size-min room-size-min
                  :room-size-max room-size-max
                  :room-density room-density
                  :door-rate door-rate
                  :windiness windiness)
  (create-walls)
  (create-rooms)
  (create-corridors)
  (create-connectors)
  (combine-dungeon)
  (remove-dead-ends))

(defun create-walls ()
  (with-slots (width height tile-map) *dungeon*
    (loop for x below width
          do (loop for y below height
                   do (setf (aref tile-map x y) (make-tile x y))))))

(defun create-rooms ()
  (loop with max-rooms = (calculate-room-count (room-density *generator*))
        with tries = 0
        until (or (= (length (rooms *dungeon*)) max-rooms)
                  (>= tries 1000))
        do (create-room)
           (incf tries)))

(defun create-corridors ()
  (on-tile-map #'carvablep #'walkablep #'carve :start '(1 1) :end '(-1 -1)))

(defun create-connectors ()
  (on-tile-map #'possible-connector-p #'region-id #'add-connector :start '(1 1) :end '(-1 -1)))

(defun combine-dungeon ()
  (with-slots (regions) *dungeon*
    (loop with region-id = (rng 'elt :list (hash-table-keys regions))
          while (connectors (gethash region-id regions))
          for door = (random-connector region-id)
          do (merge-region region-id door))))

(defun remove-dead-ends ()
  (with-slots (dead-ends-p) *dungeon*
    (loop while dead-ends-p
          do (setf dead-ends-p nil)
             (on-tile-map #'dead-end-p #'walkablep #'make-wall :start '(1 1) :end '(-1 -1)))))
