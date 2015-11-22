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
    (elt connectors (random (length connectors)))))

(defun open-connector (region-id)
  (let ((connector (random-connector region-id)))
    (setf (terrain connector) :door)
    connector))

(defun get-connected-region (region-id connector)
  (first (remove region-id (connectorp connector))))

(defun draw-merged (region-id)
  (dolist (tile (tiles (get-region region-id)))
    (setf (terrain tile) :region)))

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

(defun merge-region (region-id)
  (let* ((connector (open-connector region-id))
         (connected (get-connected-region region-id connector)))
    (setf (region-id connector) connected)
    (if (< (random 1.0) 0.05)
        (merge-region region-id)
        (progn
          (remove-extra-connectors region-id connected)
          (move-connectors connected region-id)))))

(defun merge-all ()
  (with-slots (regions) *dungeon*
    (let* ((region-count (length (hash-table-keys regions)))
           (region (gethash (1+ (random region-count)) regions)))
      (loop while (connectors region)
            do (merge-region (id region))))))
