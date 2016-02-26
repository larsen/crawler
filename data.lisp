(in-package :crawler)

(defvar *data* nil)

(defclass data ()
  ((id :reader id
       :initarg :id)
   (attrs :accessor attrs
          :initarg :attrs
          :initform nil)))

(defun data (id)
  (gethash id *data*))

(defun (setf data) (data id)
  (setf (gethash id *data*) data))

(defun load-data ()
  (setf *data* (make-hash-table))
  (loop :with file = (get-path "data" "dungeon.lisp")
        :for (id . (attrs)) :in (read-file file)
        :do (setf (data id) (make-data id attrs))))

(defun make-data (id attrs)
  (let ((data (make-instance 'data :id id)))
    (loop :for (name . attr) :in attrs
          :do (setf (attr data name) attr))
    data))

(defmethod attrs-plist (data)
  (loop :for (attr . value) :in (slot-value (data data) 'attrs)
        :collect (make-keyword attr)
        :collect value))

(defmethod attr (data name)
  (when-let ((data (data data)))
    (attr data name)))

(defmethod attr ((data data) name)
  (with-slots (attrs) data
    (cdr (assoc name attrs))))

(defmethod (setf attr) (value data name)
  (when-let ((data (data data)))
    (setf (attr data name) value)))

(defmethod (setf attr) (value (data data) name)
  (with-slots (attrs) data
    (if-let ((cell (assoc name attrs)))
      (setf (cdr cell) value)
      (progn
        (push (cons name value) attrs)
        value))))

(defmacro with-attrs (attrs data &body body)
  `(symbol-macrolet
       (,@(loop :for attr :in attrs
                :collect (list attr `(attr ,data ,(make-keyword attr)))))
     ,@body))
