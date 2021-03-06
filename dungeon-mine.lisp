(in-package :crawler)

(defclass mine (dungeon)
  ((rooms :accessor rooms
          :initform nil)
   (regions :accessor regions
            :initform (make-hash-table))
   (current-region :accessor current-region
                   :initform 0)))

(defmethod make-buffers ((type (eql :mine)))
  (with-attrs (width height) :dungeon
    (with-attrs (room-size-max) :mine
      (let ((min-size (* room-size-max 2)))
        (when (< width min-size) (setf width min-size))
        (when (< height min-size) (setf height min-size))
        (when (evenp width) (incf width))
        (when (evenp height) (incf height))))))

(defun create-rooms ()
  (loop :with max-rooms = (calculate-room-count (attr :mine :room-density))
        :with tries = 0
        :until (or (= (length (rooms *dungeon*)) max-rooms)
                   (>= tries 1000))
        :do (create-room)
            (incf tries)))

(defun create-junctions ()
  (with-slots (regions) *dungeon*
    (loop :with region-id = (rng 'elt :list (hash-table-keys regions))
          :while (connectors (gethash region-id regions))
          :for connector = (random-connector region-id)
          :for connected = (get-connected-region region-id connector)
          :do (make-junction connector)
              (remove-extra-connectors region-id connected)
              (move-connectors connected region-id))
    (map-tiles #'connectorp #'region-id #'make-extra-junction)))

(defmethod build ((type (eql :mine)) &key)
  (create-rooms)
  (map-tiles #'carvablep #'walkablep #'carve)
  (map-tiles #'connectorp #'region-id #'make-connector)
  (create-junctions)
  (process-tiles #'dead-end-p #'walkablep #'erode-dead-end)
  (create-downstairs (create-upstairs)))
