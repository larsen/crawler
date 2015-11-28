(in-package :crawler-examples)

(defparameter *draw-modes* '(walkable region))

(defsketch random-dungeon (:title "Dungeon"
                           :width (* (tile-size *dungeon*) (width *dungeon*))
                           :height (* (tile-size *dungeon*) (height *dungeon*))
                           :debug :scancode-grave)
    ((updatedp nil))
  (when (not updatedp)
    (with-slots (width height tile-size) *dungeon*
      (dotimes (x width)
        (dotimes (y height)
          (draw-tile (first *draw-modes*) x y))))
    (setf updatedp t)))

(define-sketch-setup random-dungeon
  (background (gray 0)))

(defmethod draw-tile (attr x y)
  (with-slots (tile-size) *dungeon*
    (rect (* x tile-size)
          (* y tile-size)
          (- tile-size 1)
          (- tile-size 1))))

(defmethod draw-tile ((attr (eql 'walkable)) x y)
  (macrolet ((select-pen (x y &body body)
               `(with-pen (if (walkablep (aref (tile-map *dungeon*) ,x ,y))
                              (make-pen :fill (rgb 0.1 0.5 1))
                              (make-pen :fill (gray 0.2)))
                  ,@body)))
    (select-pen x y (call-next-method))))

(defmethod draw-tile ((attr (eql 'region)) x y)
  (macrolet ((select-pen (x y &body body)
               `(let ((region (region-id (aref (tile-map *dungeon*) ,x ,y))))
                  (with-pen (make-pen :fill (if region (hash-color region) (gray 0.2)))
                    ,@body))))
    (select-pen x y (call-next-method))))

(defmethod mousebutton-event ((window random-dungeon) state ts button x y)
  (with-slots (width height tile-size room-size room-density door-rate windiness) *dungeon*
    (when (eq state :MOUSEBUTTONUP)
      (setf (slot-value window 'updatedp) nil)
      (when (eql button 1)
        (make-dungeon :w width
                      :h height
                      :tile-size tile-size
                      :room-size room-size
                      :room-density room-density
                      :door-rate door-rate
                      :windiness windiness))
      (when (eql button 3)
        (setf *draw-modes* (rotate *draw-modes*))))))

(defmethod keyboard-event ((window random-dungeon) state ts repeat-p keysym)
  (when (eq state :KEYDOWN)
    (case (sdl2:scancode keysym)
      (:scancode-escape (close-window window)))))

(defun random-dungeon (width height &key
                                      (tile-size 10)
                                      (room-size '(3 11))
                                      (room-density 0.75)
                                      (door-rate 0.1)
                                      (windiness 0)
                                      seed)
  (make-dungeon :w width
                :h height
                :tile-size tile-size
                :room-size room-size
                :room-density room-density
                :door-rate door-rate
                :windiness windiness
                :seed seed)
  (make-instance 'random-dungeon))
