(in-package :crawler)

(defvar *game* nil)

(setf *prototype-path* (get-path "proto" "proto.dat"))

(defclass game ()
  ((window :accessor window
           :initform nil)))

(defun init-game ()
  (setf *game* (make-instance 'game))
  (load-prototypes)
  (sdl2.kit:start)
  (sdl2:in-main-thread ()
    (create-window)))

(defun kill-game ()
  (when-let ((window (window *game*)))
    (format t "Quitting ~A~%" (attr (prototype 'window) 'title))
    (close-window window)))

(defun equipped (entity location)
  (attr (component entity '(equipment)) location))

(defun (setf equipped) (item entity location)
  (setf (attr (component entity '(equipment)) location) item))

(defun test ()
  (let ((human (make-entity (prototype 'creature))))
    (setf (equipped human 'left-hand) 'shield
          (equipped human 'right-hand) 'dagger)
    (format t "Human's left hand: ~A~%" (equipped human 'left-hand))
    (format t "Human's right hand: ~A~%" (equipped human 'right-hand))))
