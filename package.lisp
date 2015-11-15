(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:ax.misc.fs
        #:agl
        #:alexandria
        #:sdl2.kit))

(in-package :crawler)

(setf *system-name* :crawler)
