(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:alexandria)
  (:export #:make-dungeon
           #:*dungeon*
           #:width
           #:height
           #:data
           #:tile-size
           #:terrain
           #:region-id
           #:connectorp))

(in-package :crawler)

(setf *system-name* :crawler)
