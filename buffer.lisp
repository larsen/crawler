(in-package :crawler)

(defmethod make-buffers :around (type)
  (with-attrs (width height) :dungeon
    (with-slots (buffers tiles) *dungeon*
      (call-next-method)
      (setf tiles (make-array `(,width ,height ,(length buffers)))))))

(defun current-buffer ()
  (first (buffers *dungeon*)))

(defun next-buffer ()
  (first (rotate (copy-seq (buffers *dungeon*)) -1)))

(defun swap-buffers ()
  (with-slots (buffers) *dungeon*
    (setf buffers (rotate buffers -1))))
