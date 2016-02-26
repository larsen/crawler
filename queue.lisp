(in-package :crawler)

(defstruct (queue)
  (items)
  (tail))

(defun queue-empty-p (queue)
  (endp (queue-items queue)))

(defun enqueue (item queue)
  (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))
  queue)

(defun dequeue (queue)
  (unless (queue-empty-p queue)
    (pop (queue-items queue))))
