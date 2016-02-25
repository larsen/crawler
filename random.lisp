(in-package :crawler)

(defun make-seed ()
  (mod
   (parse-integer
    (shuffle
     (format nil "~d~d"
             (get-universal-time)
             (get-internal-real-time))))
   (expt 2 48)))

(defun make-generator (type attrs)
  (setf (random-seed *random-generator*) (make-seed))
  (loop :for (attr . value) :in (plist-alist attrs)
        :do (setf (attr type attr) value)))

(defgeneric rng (type &key))

(defmethod rng ((type (eql 'elt)) &key list)
  (random-element *random-generator* list))

(defmethod rng ((type (eql 'range-int)) &key (min 0) (max 1))
  (integer-random *random-generator* min max))

(defmethod rng ((type (eql 'range-inc)) &key (min 0.0) (max 1.0))
  (random-range-inclusive *random-generator* min max))

(defmethod rng ((type (eql 'range-odd)) &key (min 1) (max 3))
  (when (evenp min) (decf min))
  (let ((num (rng 'range-inc :min min :max max)))
    (if (evenp num) (decf num) num)))

(defmethod rng ((type (eql 'boolean)) &key (probability 0.5))
  (random-boolean *random-generator* probability))
