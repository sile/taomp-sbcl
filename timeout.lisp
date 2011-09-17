(defstruct timeout
  (:use :common-lisp)
  (:export make
           expired?))
(in-package timeout)

(defstruct timeout
  (limit 0 :type fixnum))


(defun make (timeout)
  (make-timeout :limit (+ (get-internal-real-time) timeout)))
                
(defun expired? (timeout-obj)
  (with-slots (limit) (the timeout timeout-obj)
    (< (get-internal-real-time) limit)))
