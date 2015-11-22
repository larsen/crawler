(in-package :cl-user)

(defpackage #:crawler-examples
  (:use #:cl
        #:sdl2.kit
        #:sketch)
  (:import-from #:alexandria
                #:rotate)
  (:import-from #:crawler
                #:*dungeon*
                #:make-dungeon
                #:tile-size
                #:w
                #:h
                #:data
                #:terrain
                #:region-id
                #:connectorp)
  (:export #:random-dungeon))

(in-package :crawler-examples)
