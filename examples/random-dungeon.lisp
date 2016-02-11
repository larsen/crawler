(in-package :crawler-examples)

(defsketch random-dungeon (:title "Dungeon"
                           :width (* 10 (width *dungeon*))
                           :height (* 10 (height *dungeon*))
                           :debug :scancode-grave)
    ()
  (with-slots (width height) *dungeon*
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile x y)))))

(define-sketch-setup random-dungeon
  (background (gray 0)))

(defmethod select-color (x y)
  (let ((tile (tile x y)))
    (cond
      ((eq (map-feature tile) :stairs-up)
       (rgb 1 0 0))
      ((eq (map-feature tile) :stairs-down)
       (rgb 0 1 0))
      ((eq (map-feature tile) :junction)
       (rgb 0.1 0.5 1))
      ((walkablep tile)
       (gray 1))
      (t (gray 0.2)))))

(defmethod draw-tile (x y)
  (with-pen (make-pen :fill (select-color x y))
    (rect (* x 10) (* y 10) 9 9)))

(defmethod mousebutton-event ((window random-dungeon) state ts button x y)
  (with-slots (width height) *dungeon*
    (when (eq state :MOUSEBUTTONUP)
      (when (eql button 1)
        (apply #'make-dungeon width height (attrs-plist :dungeon))))))

(defun random-dungeon (w h &rest attrs)
  (when (apply #'make-dungeon w h attrs)
    (make-instance 'random-dungeon)))
