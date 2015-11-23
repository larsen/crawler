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
    (first (shuffle connectors))))

(defun get-connected-region (region-id connector)
  (first (remove region-id (connectorp connector))))

(defun remove-extra-connectors (region-id connected-id)
  (let ((region (get-region region-id))
        (connected (get-region connected-id)))
    (dolist (connector (connectors region))
      (with-slots (connectorp) connector
        (when (and (member region-id connectorp)
                   (member connected-id connectorp))
          (setf connectorp nil)
          (deletef (connectors region) connector)
          (deletef (connectors connected) connector))))))

(defun move-connectors (from to)
  (let ((from-region (get-region from))
        (to-region (get-region to)))
    (dolist (connector (connectors from-region))
      (with-slots (connectorp) connector
        (setf connectorp (substitute to from connectorp))
        (push connector (connectors to-region))))))

(defun adjacent-door-p (tile)
  (with-slots (data) *dungeon*
    (with-slots (x y) tile
      (or (eq (terrain (aref data x (1- y))) :door)
          (eq (terrain (aref data x (1+ y))) :door)
          (eq (terrain (aref data (1- x) y)) :door)
          (eq (terrain (aref data (1+ x) y)) :door)))))

(defun merge-region (region-id door chance)
  (let* ((connected (get-connected-region region-id door))
         (extra-door (random-connector region-id)))
    (unless (adjacent-door-p door)
      (setf (region-id door) connected
            (terrain door) :door)
      (if (and (< (random 1.0) chance)
               (not (adjacent-door-p extra-door)))
          (merge-region region-id extra-door chance)
          (progn
            (remove-extra-connectors region-id connected)
            (move-connectors connected region-id))))))

(defun merge-regions (extra-door-chance)
  (with-slots (regions) *dungeon*
    (let* ((region-count (length (hash-table-keys regions)))
           (region (gethash (1+ (random region-count)) regions)))
      (loop while (connectors region)
            for door = (random-connector (id region))
            do (merge-region (id region) door extra-door-chance)))))
