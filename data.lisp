(in-package :crawler)

(defvar *prototypes* nil)

(defclass prototype ()
  ((id :reader id
       :initarg :id)
   (attrs :accessor attrs
          :initarg :attrs
          :initform nil)
   (components :accessor components
               :initarg :components
               :initform nil)))

(defun prototype (id)
  "Retrieve a prototype by its ID from the manager."
  (gethash id *prototypes*))

(defun (setf prototype) (prototype id)
  "Store a prototype with a reference of ID in the manager."
  (setf (gethash id *prototypes*) prototype))

(defun add-prototypes (file)
  "Read the prototype definition file and store each prototype in the manager."
  (loop :for (id . (attrs components)) :in (read-file file)
        :do (setf (prototype id) (make-prototype id attrs components))))

(defun load-prototypes (files)
  "Load the base prototypes and all the given prototypes."
  (setf *prototypes* (make-hash-table))
  (loop :for file :in files
        :for path = (get-path "proto" (format nil "~(~A~).proto" file))
        :do (add-prototypes path)))

(defun make-prototype (id attrs components)
  (let ((prototype (make-instance 'prototype :id id :components components)))
    (copy-components prototype)
    (add-attrs prototype attrs)
    prototype))

(defun add-attrs (to attrs)
  (loop :for (name . attr) :in attrs
        :for value = (if (typep attr 'sequence)
                         (copy-seq attr)
                         attr)
        :do (setf (attr to name) value)))

(defun copy-components (to &key from)
  (loop :for id :in (components (or from to))
        :for component = (prototype id)
        :unless (member id (components to))
          :do (push id (components to))
        :when component
          :do (copy-components to :from component)
              (add-attrs to (attrs component))))

(defmethod attrs-plist (prototype)
  (loop :for (attr . value) :in (slot-value (prototype prototype) 'attrs)
        :collect (make-keyword attr)
        :collect value))

(defmethod attr (prototype name)
  (when-let ((prototype (prototype prototype)))
    (attr prototype name)))

(defmethod attr ((prototype prototype) name)
  (with-slots (attrs) prototype
    (cdr (assoc name attrs))))

(defmethod (setf attr) (value prototype name)
  (when-let ((prototype (prototype prototype)))
    (setf (attr prototype name) value)))

(defmethod (setf attr) (value (prototype prototype) name)
  (with-slots (attrs) prototype
    (if-let ((cell (assoc name attrs)))
      (setf (cdr cell) value)
      (progn
        (push (cons name value) attrs)
        value))))

(defmacro with-attrs (attrs prototype &body body)
  `(symbol-macrolet (,@(loop :for attr :in attrs
                             :collect (list attr `(attr ,prototype ,(make-keyword attr)))))
     ,@body))
