(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)
   (rooms :accessor rooms
          :initform nil)
   (regions :accessor regions
            :initform (make-hash-table))
   (current-region :accessor current-region
                   :initform 0)
   (tile-map :accessor tile-map
             :initarg :tile-map)))

(defun make-dungeon (w h &rest attrs)
  "Top-level dungeon creator."
  (make-generator attrs)
  (when (and (oddp w)
             (oddp h)
             (>= w (* (attr 'room-size-max) 2))
             (>= h (* (attr 'room-size-max) 2)))
    (setf *dungeon* (make-instance 'dungeon
                                   :w w
                                   :h h
                                   :tile-map (make-array `(,w ,h))))
    (when (attr 'debugp)
      (format t "Random seed: ~a~%" (random-seed *generator*)))
    (generate-dungeon)))

(defun generate-dungeon ()
  "Generate all parts of the dungeon."
  (create-walls)
  (create-rooms)
  (create-corridors)
  (create-connectors)
  (create-junctions)
  (remove-dead-ends)
  (create-stairs)
  *dungeon*)

(defun create-walls ()
  "Fill the dungeon with all wall tiles."
  (with-slots (width height) *dungeon*
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
  (map-tiles #'carvablep #'walkablep #'carve))

(defun create-connectors ()
  "Mark all tiles that can connect two different regions."
  (map-tiles #'connectorp #'region-id #'make-connector))

(defun remove-dead-ends ()
  "Remove all dead-end tiles, resulting in corridors that only lead to rooms and other corridors."
  (process-tiles #'dead-end-p #'walkablep #'erode-dead-end))
