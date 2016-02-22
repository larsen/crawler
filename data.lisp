(in-package :crawler)

(defvar *data* nil)

(defclass data ()
  ((id :reader id
       :initarg :id)
   (attrs :accessor attrs
          :initarg :attrs
          :initform nil)
   (components :accessor components
               :initarg :components
               :initform nil)))

(defun data (id)
  "Retrieve data by its ID from the manager."
  (gethash id *data*))

(defun (setf data) (data id)
  "Store data with a reference of ID in the manager."
  (setf (gethash id *data*) data))

(defun load-data ()
  "Read a data file and store each item in the manager."
  (setf *data* (make-hash-table))
  (loop :with file = (get-path "data" "dungeon.lisp")
        :for (id . (attrs components)) :in (read-file file)
        :do (setf (data id) (make-data id attrs components))))

(defun make-data (id attrs components)
  (let ((data (make-instance 'data :id id :components components)))
    (copy-components data)
    (add-attrs data attrs)
    data))

(defun add-attrs (to attrs)
  (loop :for (name . attr) :in attrs
        :for value = (if (typep attr 'sequence)
                         (copy-seq attr)
                         attr)
        :do (setf (attr to name) value)))

(defun copy-components (to &key from)
  (loop :for id :in (components (or from to))
        :for component = (data id)
        :unless (member id (components to))
          :do (push id (components to))
        :when component
          :do (copy-components to :from component)
              (add-attrs to (attrs component))))

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
