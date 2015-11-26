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
  (gethash id (regions *dungeon*)))

(defun random-connector (region-id)
  (with-slots (connectors) (get-region region-id)
    (random-elt connectors)))

(defun get-connected-region (region-id tile)
  (first (remove region-id (gethash tile (connectors *dungeon*)))))

(defun remove-extra-connectors (region-id connected-id)
  (let ((region (get-region region-id))
        (connected (get-region connected-id)))
    (dolist (tile (connectors region))
      (let ((connector (gethash tile (connectors *dungeon*))))
        (when (and (member region-id connector)
                   (member connected-id connector))
          (deletef (connectors region) tile)
          (deletef (connectors connected) tile))))))

(defun move-connectors (from to)
  (let ((from-region (get-region from))
        (to-region (get-region to)))
    (dolist (tile (connectors from-region))
      (setf (gethash tile (connectors *dungeon*))
            (substitute to from (gethash tile (connectors *dungeon*))))
      (push tile (connectors to-region)))))

(defun adjacent-door-p (tile)
  (with-slots (tile-map doors) *dungeon*
    (with-slots (x y) tile
      (or (member (aref tile-map x (1- y)) doors)
          (member (aref tile-map x (1+ y)) doors)
          (member (aref tile-map (1- x) y) doors)
          (member (aref tile-map (1+ x) y) doors)))))

(defun merge-region (region-id door)
  (let* ((connected (get-connected-region region-id door))
         (extra-door (random-connector region-id)))
    (unless (adjacent-door-p door)
      (setf (region-id door) 0
            (walkablep door) t)
      (push door (doors *dungeon*))
      (if (< (random 1.0) (door-rate *dungeon*))
          (merge-region region-id extra-door)
          (progn
            (remove-extra-connectors region-id connected)
            (move-connectors connected region-id))))))
