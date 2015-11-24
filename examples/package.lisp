(in-package :cl-user)

(defpackage #:crawler-examples
  (:use #:cl
        #:sdl2.kit
        #:crawler)
  (:import-from #:sketch
                #:defsketch
                #:define-sketch-setup
                #:rect
                #:with-pen
                #:make-pen
                #:hsb-360
                #:gray
                #:rgb
                #:background
                #:ellipse)
  (:import-from #:alexandria
                #:rotate)
  (:export #:random-dungeon))

(in-package :crawler-examples)
