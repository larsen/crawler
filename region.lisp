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
  (rng 'elt :list (connectors (get-region region-id))))

(defun get-connected-region (region-id tile)
  (first (remove region-id (gethash tile (connectors *dungeon*)))))

(defun remove-extra-connectors (region-id connected-id)
  (let ((region (get-region region-id))
        (connected (get-region connected-id)))
    (with-slots (connectors) *dungeon*
      (dolist (tile (connectors region))
        (let ((connector (gethash tile connectors)))
          (when (and (member region-id connector)
                     (member connected-id connector))
            (deletef (connectors region) tile)
            (deletef (connectors connected) tile)
            (remhash tile connectors)))))))

(defun move-connectors (from to)
  (let ((from-region (get-region from))
        (to-region (get-region to)))
    (with-slots (connectors) *dungeon*
      (dolist (tile (connectors from-region))
        (setf (gethash tile connectors)
              (substitute to from (gethash tile connectors)))
        (push tile (connectors to-region))))))

(defun adjacent-door-p (tile)
  (with-slots (tile-map doors) *dungeon*
    (with-slots (x y) tile
      (or (member (tile x (1- y)) doors)
          (member (tile x (1+ y)) doors)
          (member (tile (1- x) y) doors)
          (member (tile (1+ x) y) doors)))))

(defun merge-region (region-id door)
  (let* ((connected (get-connected-region region-id door))
         (extra-door (random-connector region-id)))
    (unless (adjacent-door-p door)
      (setf (region-id door) 0
            (walkablep door) t)
      (push door (doors *dungeon*))
      (if (< (rng 'range-i) (attr 'door-rate))
          (merge-region region-id extra-door)
          (progn
            (remove-extra-connectors region-id connected)
            (move-connectors connected region-id))))))
