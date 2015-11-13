(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:ax.misc.fs
        #:agl
        #:sketch
        #:sdl2.kit)
  (:import-from #:alexandria
                #:when-let)
)

(in-package :crawler)

(setf *system-name* :crawler)
