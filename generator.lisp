(in-package :crawler)

(defun init-generator (seed)
  (let ((new-seed (parse-integer
                   (format nil "~d~d"
                           (get-universal-time)
                           (get-internal-real-time)))))
    (setf *random-generator* (make-random-number-generator (mod new-seed (expt 2 48))))
    (when seed
      (setf (random-seed *random-generator*) seed))
    (format t "Random seed: ~a~%" (random-seed *random-generator*))))

(defgeneric rng (type &key))

(defmethod rng ((type (eql 'elt)) &key list)
  (random-element *random-generator* list))

(defmethod rng ((type (eql 'range-i)) &key (min 0) (max 1))
  (random-range-inclusive *random-generator* min max))

(defmethod rng ((type (eql 'odd-range)) &key (min 1) (max 3))
  (when (evenp min) (decf min))
  (let ((n (random-range-inclusive *random-generator* min max)))
    (if (evenp n) (decf n) n)))

(defmethod rng ((type (eql 'int)) &key (min 0) (max 1))
  (integer-random *random-generator* min max))
