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
   (doors :accessor doors
          :initform nil)
   (connectors :accessor connectors
               :initform (make-hash-table))
   (tile-map :accessor tile-map
             :initarg :tile-map)))

(defun make-dungeon (w h tile-size &rest attrs)
  "Top-level dungeon creator."
  (when (and (oddp w)
             (oddp h)
             (>= w 9)
             (>= h 9))
    (setf *dungeon* (make-instance 'dungeon
                                   :w w
                                   :h h
                                   :tile-size tile-size
                                   :tile-map (make-array (* w h))))
    (generate attrs)))

(defun generate (attrs)
  "Generate all parts of the dungeon."
  (make-generator attrs)
  (create-walls)
  (create-rooms)
  (create-corridors)
  (create-connectors)
  (combine-regions)
  (remove-dead-ends)
  *dungeon*)

(defun create-walls ()
  "Fill the dungeon with all wall tiles."
  (with-slots (width height tile-map) *dungeon*
    (loop for x below width
          do (loop for y below height
                   do (setf (tile x y) (make-tile x y))))))

(defun create-rooms ()
  "Create rooms until the desired density is reached."
  (loop with max-rooms = (calculate-room-count (attr 'room-density))
        with tries = 0
        until (or (= (length (rooms *dungeon*)) max-rooms)
                  (>= tries 1000))
        do (create-room)
           (incf tries)))

(defun create-corridors ()
  "Carve out corridors from the remaining walls around rooms."
  (on-tile-map #'carvablep #'walkablep #'carve :start '(1 1) :end '(-1 -1)))

(defun create-connectors ()
  "Mark all tiles that can connect two different regions."
  (on-tile-map #'connectorp #'region-id #'make-connector :start '(1 1) :end '(-1 -1)))

(defun combine-regions ()
  "Join all regions by carving some connectors."
  (with-slots (regions) *dungeon*
    (loop with region-id = (rng 'elt :list (hash-table-keys regions))
          while (connectors (gethash region-id regions))
          for door = (random-connector region-id)
          do (merge-region region-id door))))

(defun remove-dead-ends ()
  "Remove all dead-end tiles, resulting in corridors that only lead to rooms and other corridors."
  (converge (on-tile-map #'dead-end-p #'walkablep #'make-wall :start '(1 1) :end '(-1 -1))))
