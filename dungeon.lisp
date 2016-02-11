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

(defun set-attrs (attrs)
  "Set the specified generator attributes."
  (loop :for (attr . value) :in (plist-alist attrs)
        :do (setf (attr :dungeon attr) value)))

(defun make-tile-map (w h)
  (setf *dungeon* (make-instance 'dungeon :w w :h h))
  (with-slots (width height tile-map) *dungeon*
    (with-attrs (room-size-max) :dungeon
      (let ((min-size (* room-size-max 2)))
        (when (< width min-size) (setf width min-size))
        (when (< height min-size) (setf height min-size))
        (when (evenp width) (incf width))
        (when (evenp height) (incf height)))
      (setf tile-map (make-array `(,width ,height))))))

(defun make-dungeon (w h &rest attrs)
  "Top-level dungeon creator."
  (load-prototypes '("dungeon"))
  (make-generator)
  (set-attrs attrs)
  (make-tile-map w h)
  (generate-dungeon))

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
    (loop :for x :below width
          do (loop :for y :below height
                   :do (setf (tile x y) (make-tile x y))))))

(defun create-rooms ()
  "Create rooms until the desired density is reached."
  (loop :with max-rooms = (calculate-room-count (attr :dungeon :room-density))
        :with tries = 0
        :until (or (= (length (rooms *dungeon*)) max-rooms)
                   (>= tries 1000))
        :do (create-room)
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
