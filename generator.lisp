(in-package :crawler)

(defclass generator (random-number-generation-mixin)
  ((debugp :accessor debugp
           :initform nil)
   (windiness :accessor windiness
              :initform 0)
   (room-density :accessor room-density
                 :initform 0.65)
   (room-size-min :accessor room-size-min
                  :initform 3)
   (room-size-max :accessor room-size-max
                  :initform 11)
   (junction-rate :accessor junction-rate
                  :initform 0.03)))

(defun attr (name)
  "Get a generator attribute value given its name."
  (funcall name *generator*))

(defun (setf attr) (value name)
  "Set the value of a generator attribute of the specified name."
  (when (slot-exists-p *generator* name)
    (setf (slot-value *generator* name)
          (or value (slot-value *generator* name)))))

(defun get-attrs ()
  "Get a list of all generator attributes and their values."
  (list :debugp (attr 'debugp)
        :windiness (attr 'windiness)
        :room-density (attr 'room-density)
        :room-size-min (attr 'room-size-min)
        :room-size-max (attr 'room-size-max)
        :junction-rate (attr 'junction-rate)))

(defun set-attrs (attrs)
  "Set the specified generator attributes."
  (loop for (attr . value) in (plist-alist attrs)
        for name = (intern (symbol-name attr) :crawler)
        do (setf (attr name) value)))

(defun make-generator (attrs)
  "Create a new generator."
  (setf *generator* (make-instance 'generator))
  (set-attrs attrs)
  (set-seed (getf attrs :seed))
  *generator*)
