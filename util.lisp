(in-package :crawler)

(defmacro converge (&body body)
  `(loop :while ,@body))

(defun get-path (dir file)
  (let ((path (format nil "~A/~@[~A~]" dir file)))
    (asdf/system:system-relative-pathname :crawler path)))

(defun read-file (file-path)
  (let ((*read-eval* nil))
    (with-open-file (in file-path :direction :input)
      (read in))))

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
