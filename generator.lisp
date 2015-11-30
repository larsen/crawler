(in-package :crawler)

(defvar *generator* nil)

(defclass generator (random-number-generation-mixin)
  ((attrs :accessor attrs
          :initform '((windiness . 0)
                      (room-density . 0.65)
                      (room-size-min . 3)
                      (room-size-max . 11)
                      (door-rate . 0.1)))))

(defun attr (name)
  (cdr (assoc name (attrs *generator*))))

(defun (setf attr) (value name unpacked)
  (with-slots (attrs) *generator*
    (if-let ((cell (assoc name attrs))
             (value (or value (getf unpacked (make-keyword name)))))
      (setf (cdr cell) value)
      (progn
        (push (cons name value) attrs)
        value))))

(defun make-seed ()
  (parse-integer
   (format nil "~d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
           (mod
            (parse-integer
             (format nil "~d~d" (get-universal-time) (get-internal-real-time)))
            (expt 2 36))
           (truncate (attr 'door-rate) 0.01)
           (attr 'room-size-max)
           (attr 'room-size-min)
           (truncate (attr 'room-density) 0.01)
           (truncate (attr 'windiness) 0.01))))

(defun set-random-seed (&optional seed)
  (let ((packed (or seed (make-seed))))
    (setf (random-seed *generator*) (getf (unpack-seed packed) :seed))
    (format t "Random seed: ~a~%" packed)))

(defun seed-valid-p (seed)
  (let ((unpacked (unpack-seed seed)))
    (when (and seed (> (getf unpacked :seed) 1))
      seed)))

(defmacro set-attrs (unpacked attrs)
  `(setf
    ,@(loop for attr in attrs append
            `((attr ',attr ,unpacked) ,attr))))

(defun make-generator (&key seed room-size-min room-size-max room-density door-rate windiness)
  (setf *generator* (make-instance 'generator))
  (let* ((packed (make-seed))
         (seed (seed-valid-p seed))
         (unpacked (unpack-seed (or seed packed))))
    (set-attrs unpacked (room-size-min room-size-max room-density door-rate windiness))
    (set-random-seed seed)
    *generator*))

(defun unpack-seed (seed)
  (flet ((apply-arg (packed)
           `(,(make-keyword (caar packed))
             ,(typecase (cdar packed)
                (float (float (/ (mod seed 100) 100)))
                (integer (mod seed 100))
                (t seed)))))
    (loop with args
          with packed = (attrs *generator*)
          while (and seed packed)
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
