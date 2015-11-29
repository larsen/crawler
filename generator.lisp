(in-package :crawler)

(defvar *generator* nil)

(defclass generator (random-number-generation-mixin)
  ((windiness :accessor windiness
              :initarg :windiness
              :initform 0)
   (room-density :accessor room-density
                 :initarg :room-density
                 :initform 0.65)
   (room-size-min :accessor room-size-min
                  :initarg room-size-min
                  :initform 3)
   (room-size-max :accessor room-size-max
                  :initarg :room-size-max
                  :initform 11)
   (door-rate :accessor door-rate
              :initarg :door-rate
              :initform 0.05)))

(defun make-seed ()
  (parse-integer
   (format nil "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
           (mod
            (parse-integer
             (format nil "~d~d" (get-universal-time) (get-internal-real-time)))
            (expt 2 36))
           (truncate (windiness *generator*) 0.01)
           (truncate (door-rate *generator*) 0.01)
           (truncate (room-density *generator*) 0.01)
           (room-size-max *generator*)
           (room-size-min *generator*))))

(defun set-attr (unpacked name value)
  (setf (slot-value *generator* name)
        (or value
            (getf unpacked (make-keyword name))
            (slot-value *generator* name))))

(defun set-random-seed (&optional seed)
  (let ((packed (or seed (make-seed))))
    (setf (random-seed *generator*) (getf (unpack-seed packed) :seed))
    (format t "Random seed: ~a~%" packed)))

(defun make-generator (&key seed room-size-min room-size-max room-density door-rate windiness)
  (setf *generator* (make-instance 'generator))
  (let* ((packed (make-seed))
         (unpacked (unpack-seed (or seed packed))))
    (set-attr unpacked 'room-size-min room-size-min)
    (set-attr unpacked 'room-size-max room-size-max)
    (set-attr unpacked 'room-density room-density)
    (set-attr unpacked 'door-rate door-rate)
    (set-attr unpacked 'windiness windiness)
    (set-random-seed seed)
    *generator*))

(defun unpack-seed (seed)
  (flet ((apply-arg (packed)
           `(,(cadar packed)
             ,(case (caar packed)
                (:float (float (/ (mod seed 100) 100)))
                (:int (mod seed 100))))))
    (loop with args
          with packed = '((:int :room-size-min)
                          (:int :room-size-max)
                          (:float :room-density)
                          (:float :door-rate)
                          (:float :windiness))
          while packed
          do (appendf args (apply-arg packed))
             (setf seed (truncate (/ seed 100)))
             (pop packed)
          finally (return (append `(:seed ,seed) args)))))

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
