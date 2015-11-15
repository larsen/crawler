(in-package :crawler-examples)

(defparameter *draw-mode* '(terrain region))

(defsketch random-dungeon (:title "Dungeon"
                           :width (* (tile-size *dungeon*) (w *dungeon*))
                           :height (* (tile-size *dungeon*) (h *dungeon*))
                           :debug :scancode-grave)
    ()
  (with-slots (w h tile-size) *dungeon*
    (dotimes (x w)
      (dotimes (y h)
        (draw-tile (first *draw-mode*) x y)))))

(defmethod draw-tile (attr x y)
  (with-slots (tile-size) *dungeon*
    (rect (* x tile-size)
          (* y tile-size)
          (* (1+ x) tile-size)
          (* (1+ y) tile-size))))

(defmethod draw-tile ((attr (eql 'terrain)) x y)
  (macrolet ((select-tile (x y &body body)
               `(with-pen (case (terrain (aref (data *dungeon*) ,x ,y))
                            (:corridor (make-pen :stroke (gray 0) :fill (gray 1)))
                            (:room (make-pen :stroke (gray 0) :fill (rgb 0.1 0.5 1)))
                            (:wall (make-pen :fill (gray 0))))
                  ,@body)))
    (select-tile x y (call-next-method))))

(defmethod draw-tile ((attr (eql 'region)) x y)
  (macrolet ((select-tile (x y &body body)
               `(let* ((region (region (aref (data *dungeon*) ,x ,y)))
                       (color (if (= region 0)
                                  (gray 0)
                                  (hsb-360 (* 10 (mod region 36))
                                           (* 50 (1+ (mod region 2)))
                                           (* 50 (1+ (mod region 2)))))))
                  (with-pen (make-pen :stroke (gray 0) :fill color)
                    ,@body))))
    (select-tile x y (call-next-method))))

(defmethod mousebutton-event ((window random-dungeon) state ts button x y)
  (with-slots (w h tile-size) *dungeon*
    (when (eq state :MOUSEBUTTONUP)
      (when (eql button 1)
        (make-dungeon :w w :h h :tile-size tile-size :density (+ 0.1 (random (- 0.75 0.1)))))
      (when (eql button 3)
        (setf *draw-mode* (rotate *draw-mode* -1))))))

(defun random-dungeon (width height tile-size)
  (make-dungeon :w width :h height :tile-size tile-size)
  (make-instance 'random-dungeon))
