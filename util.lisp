(in-package :crawler)

(defmacro converge (&body body)
  "Convenience macro for looping over body until it does not return true."
  `(loop while ,@body))

#+sbcl
(defmacro profile (width height &key (count 1))
  `(progn
     (sb-profile:unprofile)
     (sb-profile:profile ,(package-name *package*))
     (loop repeat ,count
           do (make-dungeon ,width ,height 10))
     (sb-profile:report)
     (sb-profile:unprofile)
     (sb-profile:reset)))
