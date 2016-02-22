(in-package :cl-user)

(defpackage #:crawler-examples
  (:use #:cl
        #:sdl2.kit
        #:crawler)
  (:import-from #:sketch
                #:defsketch
                #:rect
                #:with-pen
                #:make-pen
                #:gray
                #:rgb
                #:background)
  (:export #:run))

(in-package :crawler-examples)
