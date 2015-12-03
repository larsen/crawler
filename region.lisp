(in-package :crawler)

(defclass region ()
  ((id :reader id
       :initarg :id
       :initform nil)
   (connectors :accessor connectors
               :initform nil)
   (tiles :accessor tiles
          :initform nil)))

(defun get-region (id)
  "Get a region instance from the specified ID."
  (gethash id (regions *dungeon*)))

(defun random-connector (region-id)
  "Get a random connector tile of a region."
  (rng 'elt :list (connectors (get-region region-id))))

(defun get-connected-region (region-id tile)
  "Get the secondary region a connector tile connects."
  (first (remove region-id (adjacent-regions tile))))

(defun remove-extra-connectors (region-id connected-id)
  "Remove extra connector tiles no longer needed."
  (let ((region (get-region region-id))
        (connected (get-region connected-id)))
    (with-slots (connectors) *dungeon*
      (dolist (tile (connectors region))
        (let ((connector (adjacent-regions tile)))
          (when (and (member region-id connector)
                     (member connected-id connector))
            (deletef (connectors region) tile)
            (deletef (connectors connected) tile)))))))

(defun move-connectors (from to)
  "Move connectors of a region to the new region after it has been merged."
  (let ((from-region (get-region from))
        (to-region (get-region to)))
    (with-slots (connectors) *dungeon*
      (dolist (tile (connectors from-region))
        (setf (adjacent-regions tile)
              (substitute to from (adjacent-regions tile)))
        (push tile (connectors to-region))))))

(defun merge-region (region-id connector)
  "Merge a region with another by carving the specified connector into a junction."
  (let ((connected (get-connected-region region-id connector)))
    (make-junction connector)
    (remove-extra-connectors region-id connected)
    (move-connectors connected region-id)))
