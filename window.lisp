(in-package :crawler)

(defun make-gl-context (window-data)
  (dolist (attr `((:multisamplebuffers ,(if (attr window-data 'aa) 1 0))
                  (:multisamplesamples ,(or (attr window-data 'aa) 0))
                  (:context-major-version ,(attr window-data 'gl-major))
                  (:context-minor-version ,(attr window-data 'gl-minor))))
    (apply #'sdl2:gl-set-attr attr)))

(defun create-window ()
  (with-slots (window) *game*
    (let ((data (prototype 'window)))
      (make-gl-context data)
      (setf window (make-instance 'gl-window
                                  :title (attr data 'title)
                                  :w (attr data 'w)
                                  :h (attr data 'h))
            (idle-render window) t))))

(defmethod render ((window gl-window)))

(defmethod close-window ((window gl-window))
  (when *game*
    (setf (window *game*) nil))
  (call-next-method))
