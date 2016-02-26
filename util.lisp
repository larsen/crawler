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
