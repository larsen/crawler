(in-package :crawler)

(defmacro converge (&body body)
  "Convenience macro for looping over body until it does not return true."
  `(loop while ,@body))
