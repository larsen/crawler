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
                #:gray
                #:rgb
                #:background)
  (:export #:random-dungeon))

(in-package :crawler-examples)
