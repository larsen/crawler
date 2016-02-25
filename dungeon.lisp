(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)
   (buffers :accessor buffers
            :initform (list 0))
   (tiles :accessor tiles
          :initarg :tiles)))

(defmethod create-walls :around (&key)
  (dolist (i (buffers *dungeon*))
    (call-next-method :buffer i)))

(defmethod create-walls (&key buffer)
  (with-slots (width height) *dungeon*
    (loop :for x :below width
          do (loop :for y :below height
                   :do (setf (tile x y :buffer buffer) (make-instance 'tile :x x :y y))))))

(defgeneric build (type))

(defmethod build :around (type)
  (create-walls)
  (call-next-method))

(defun make-dungeon (type width height &rest attrs)
  (setf *dungeon* (make-instance (intern (string type) :crawler) :w width :h height))
  (load-data)
  (make-generator type attrs)
  (make-buffers type)
  (build type)
  *dungeon*)
