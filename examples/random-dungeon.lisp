(in-package :crawler-examples)

(defparameter *draw-modes* '(terrain region))

(defsketch random-dungeon (:title "Dungeon"
                           :width (* (tile-size *dungeon*) (w *dungeon*))
                           :height (* (tile-size *dungeon*) (h *dungeon*))
                           :debug :scancode-grave)
    ((updatedp nil))
  (when (not updatedp)
    (with-slots (w h tile-size) *dungeon*
      (dotimes (x w)
        (dotimes (y h)
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

(defmethod draw-tile ((attr (eql 'terrain)) x y)
  (macrolet ((select-pen (x y &body body)
               `(with-pen (case (terrain (aref (data *dungeon*) ,x ,y))
                            (:corridor (make-pen :fill (gray 1)))
                            (:door (make-pen :fill (rgb 1 0 0)))
                            (:room (make-pen :fill (rgb 0.1 0.5 1)))
                            (:wall (make-pen :fill (gray 0))))
                  ,@body)))
    (select-pen x y (call-next-method))))

(defmethod draw-tile ((attr (eql 'region)) x y)
  (macrolet ((select-pen (x y &body body)
               `(let* ((region (region-id (aref (data *dungeon*) ,x ,y)))
                       (color (if region
                                  (hsb-360 (* 10 (mod region 36))
                                           (* 50 (1+ (mod region 2)))
                                           (* 50 (1+ (mod region 2))))
                                  (gray 0))))
                  (with-pen (make-pen :fill color)
                    ,@body))))
    (select-pen x y (call-next-method))
    (draw-tile 'connector x y)))

(defmethod draw-tile ((attr (eql 'connector)) x y)
  (with-slots (data tile-size) *dungeon*
    (when (connectorp (aref data x y))
      (with-pen (make-pen :fill (rgb 1 0 0))
        (ellipse (+ (/ tile-size 2) (* x tile-size))
                 (+ (/ tile-size 2) (* y tile-size))
                 (/ tile-size 5)
                 (/ tile-size 5))))))

(defmethod mousebutton-event ((window random-dungeon) state ts button x y)
  (with-slots (w h tile-size) *dungeon*
    (when (eq state :MOUSEBUTTONUP)
      (setf (slot-value window 'updatedp) nil)
      (when (eql button 1)
        (make-dungeon :w w :h h :tile-size tile-size))
      (when (eql button 3)
        (setf *draw-modes* (rotate *draw-modes* -1))))))

(defmethod keyboard-event ((window random-dungeon) state ts repeat-p keysym)
  (when (eq state :KEYDOWN)
    (case (sdl2:scancode keysym)
      (:scancode-escape (close-window window)))))

(defun random-dungeon (width height tile-size)
  (make-dungeon :w width :h height :tile-size tile-size)
  (make-instance 'random-dungeon))
