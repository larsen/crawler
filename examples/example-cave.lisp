(in-package :crawler-examples)

(defsketch :cave (:title "Example Cave"
                  :width (* *tile-size* (attr :dungeon :width))
                  :height (* *tile-size* (attr :dungeon :height))
                  :debug :scancode-grave)
    ()
  (draw))

(defmethod regenerate ((window :cave))
  (apply #'build :cave (attrs-plist :cave)))
