(in-package :crawler-examples)

(defsketch :mine (:title "Example Mine"
                  :width (* *tile-size* (attr :dungeon :width))
                  :height (* *tile-size* (attr :dungeon :height))
                  :debug :scancode-grave)
    ()
  (draw))

(defmethod regenerate ((window :mine))
  (apply #'build :mine (attrs-plist :mine)))
