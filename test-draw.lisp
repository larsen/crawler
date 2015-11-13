(in-package :crawler)

(defsketch test-draw (:title "Dungeon"
                      :width (* (tile-size *dungeon*) (w *dungeon*))
                      :height (* (tile-size *dungeon*) (h *dungeon*)))
    ()
  (with-slots (w h tile-size) *dungeon*
    (dotimes (x w)
      (dotimes (y h)
        (draw-tile x y tile-size)))))

(defmacro select-pen (x y &body body)
  `(with-pen (case (aref (data *dungeon*) ,x ,y)
               (1 (make-pen :fill (rgb 1 1 1)))
               (2 (make-pen :fill (rgb 0.1 0.5 1)))
               (t (make-pen :fill (rgb 0 0 0))))
     ,@body))

(defun draw-tile (x y size)
  (select-pen x y
    (rect (* x size)
          (* y size)
          (* (1+ x) size)
          (* (1+ y) size))))

(defmethod mousebutton-event ((window test-draw) state ts button x y)
  (with-slots (w h tile-size) *dungeon*
    (case state
      (:MOUSEBUTTONUP (make-dungeon :w w :h h :tile-size tile-size)))))

(defun test-draw (width height tile-size)
  (make-dungeon :w width :h height :tile-size tile-size)
  (make-instance 'test-draw))
