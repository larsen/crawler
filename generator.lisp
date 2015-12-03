(in-package :crawler)

(defvar *generator* nil)

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
                  :initform 0.1)))

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

(defun make-seed ()
  "Create a random seed for the generator."
  (mod
   (parse-integer
    (shuffle
     (format nil "~d~d"
             (get-universal-time)
             (get-internal-real-time))))
   (expt 2 48)))

(defun seed-valid-p (seed)
  "Check if a random seed is valid."
  (when (and (integerp seed)
             (> seed 0))
    seed))

(defun set-seed (seed)
  "Set the random seed to be used by the generator."
  (let ((seed (or (seed-valid-p seed) (make-seed))))
    (setf (random-seed *generator*) seed)))

(defun make-generator (attrs)
  "Create a new generator."
  (setf *generator* (make-instance 'generator))
  (set-attrs attrs)
  (set-seed (getf attrs :seed))
  *generator*)

(defgeneric rng (type &key)
  (:documentation "Convenience function for generating random numbers."))

(defmethod rng ((type (eql 'elt)) &key list)
  (random-element *generator* list))

(defmethod rng ((type (eql 'range-i)) &key (min 0.0) (max 1.0))
  (random-range-inclusive *generator* min max))

(defmethod rng ((type (eql 'odd-range)) &key (min 1) (max 3))
  (when (evenp min) (decf min))
  (let ((n (random-range-inclusive *generator* min max)))
    (if (evenp n) (decf n) n)))

(defmethod rng ((type (eql 'int)) &key (min 0) (max 1))
  (integer-random *generator* min max))
