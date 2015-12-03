(in-package :crawler-examples)

(defparameter *draw-modes* '(walkable))

(defsketch random-dungeon (:title "Dungeon"
                           :width (* (tile-size *dungeon*) (width *dungeon*))
                           :height (* (tile-size *dungeon*) (height *dungeon*))
                           :debug :scancode-grave)
    ()
  (with-slots (width height) *dungeon*
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile x y)))))

(define-sketch-setup random-dungeon
  (background (gray 0)))

(defmethod draw-tile (x y)
  (let ((color (if (walkablep (tile x y)) (gray 1) (gray 0.2))))
    (with-slots (tile-size) *dungeon*
      (with-pen (make-pen :fill color)
        (rect (* x tile-size)
              (* y tile-size)
              (- tile-size 1)
              (- tile-size 1))))))

(defmethod mousebutton-event ((window random-dungeon) state ts button x y)
  (with-slots (width height tile-size) *dungeon*
    (when (eq state :MOUSEBUTTONUP)
      (when (eql button 1)
        (apply #'make-dungeon width height tile-size (get-attrs))))))

(defmethod keyboard-event ((window random-dungeon) state ts repeat-p keysym)
  (when (eq state :KEYDOWN)
    (case (sdl2:scancode keysym)
      (:scancode-escape (close-window window)))))

(defun random-dungeon (w h tile-size &rest attrs)
  (when (apply #'make-dungeon w h tile-size attrs)
    (make-instance 'random-dungeon)))
