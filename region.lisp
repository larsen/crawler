(in-package :crawler)

(defclass region ()
  ((id :reader id
       :initarg :id
       :initform nil)
   (connectors :accessor connectors
               :initform nil)))

(defun make-region ()
  "Create a new region."
  (with-slots (current-region regions) *dungeon*
    (let ((id (incf current-region)))
      (setf (gethash id regions) (make-instance 'region :id id))
      id)))

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
    (dolist (tile (connectors region))
      (let ((adjacent (adjacent-regions tile)))
        (when (and (member region-id adjacent)
                   (member connected-id adjacent))
          (deletef (connectors region) tile)
          (deletef (connectors connected) tile))))))

(defun move-connectors (from to)
  "Move connectors of a region to the new region after it has been merged."
  (let ((from-region (get-region from))
        (to-region (get-region to)))
    (dolist (tile (connectors from-region))
      (setf (adjacent-regions tile)
            (substitute to from (adjacent-regions tile)))
      (push tile (connectors to-region)))))
