(in-package :crawler)

(defvar *generator* nil)

(defclass generator (random-number-generation-mixin)
  ((windiness :accessor windiness
              :initform 0)
   (room-density :accessor room-density
                 :initform 0.65)
   (room-size-min :accessor room-size-min
                  :initform 3)
   (room-size-max :accessor room-size-max
                  :initform 11)
   (door-rate :accessor door-rate
              :initform 0.1)))

(defun attr (name)
  (funcall name *generator*))

(defun (setf attr) (value name)
  (when (slot-exists-p *generator* name)
    (setf (slot-value *generator* name)
          (or value (slot-value *generator* name)))))

(defun get-attrs ()
  (list :windiness (attr 'windiness)
        :room-density (attr 'room-density)
        :room-size-min (attr 'room-size-min)
        :room-size-max (attr 'room-size-max)
        :door-rate (attr 'door-rate)))

(defun set-attrs (attrs)
  (loop for (attr . value) in (plist-alist attrs)
        for name = (intern (symbol-name attr) :crawler)
        do (setf (attr name) value)))

(defun make-seed ()
  (mod
   (parse-integer
    (shuffle
     (format nil "~d~d"
             (get-universal-time)
             (get-internal-real-time))))
   (expt 2 48)))

(defun seed-valid-p (seed)
  (when (and seed (> seed 0))
    seed))

(defun set-seed (seed)
  (let ((seed (or (seed-valid-p seed) (make-seed))))
    (setf (random-seed *generator*) seed)
    (format t "Random seed: ~a~%" seed)))

(defun make-generator (attrs)
  (setf *generator* (make-instance 'generator))
  (set-attrs attrs)
  (set-seed (getf attrs :seed))
  *generator*)

(defgeneric rng (type &key))

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
