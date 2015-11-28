(in-package :crawler)

(defvar *generator* nil)

(defclass generator (random-number-generation-mixin)
  ((windiness :accessor windiness
              :initarg :windiness
              :initform 0)
   (room-density :accessor room-density
                 :initarg :room-density
                 :initform 0.55)
   (room-size-min :accessor room-size-min
                  :initarg room-size-min
                  :initform 3)
   (room-size-max :accessor room-size-max
                  :initarg :room-size-max
                  :initform 11)
   (door-rate :accessor door-rate
              :initarg :door-rate
              :initform 0.1)))

(defun make-seed ()
  (mod
   (parse-integer
    (format nil "~d~d"
            (get-universal-time)
            (get-internal-real-time)))
   (expt 2 24)))

(defun make-generator (&key seed room-size-min room-size-max room-density door-rate windiness)
  (setf *generator* (make-instance 'generator)
        (random-seed *generator*) (or seed (make-seed))
        (room-size-min *generator*) (or room-size-min (room-size-min *generator*))
        (room-size-max *generator*) (or room-size-max (room-size-max *generator*))
        (room-density *generator*) (or room-density (room-density *generator*))
        (door-rate *generator*) (or door-rate (door-rate *generator*))
        (windiness *generator*) (or windiness (windiness *generator*)))
  (format t "Random seed: ~a~%" (random-seed *generator*))
  *generator*)

(defun unpack-seed (seed)
  (loop with args
        with packed = '(:windiness :room-density :door-rate)
        while packed
        do (appendf args (append `(,(car packed) ,(float (/ (mod seed 100) 100)))))
           (setf seed (truncate (/ seed 100)))
           (pop packed)
        finally (return (append args `(:seed ,seed)))))

(defgeneric rng (type &key))

(defmethod rng ((type (eql 'elt)) &key list)
  (random-element *generator* list))

(defmethod rng ((type (eql 'range-i)) &key (min 0) (max 1))
  (random-range-inclusive *generator* min max))

(defmethod rng ((type (eql 'odd-range)) &key (min 1) (max 3))
  (when (evenp min) (decf min))
  (let ((n (random-range-inclusive *generator* min max)))
    (if (evenp n) (decf n) n)))

(defmethod rng ((type (eql 'int)) &key (min 0) (max 1))
  (integer-random *generator* min max))
