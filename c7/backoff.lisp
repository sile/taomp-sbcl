(defpackage backoff
  (:use :common-lisp :util)
  (:export make
           backoff
           object))
(in-package :backoff)

(defstruct backoff-obj
  (min-delay 0 :type fixnum)
  (max-delay 0 :type fixnum)
  (limit 0 :type fixnum))

(deftype object () 'backoff-obj)

(defun make (min-delay max-delay)
  (make-backoff-obj :min-delay min-delay
                    :max-delay max-delay
                    :limit min-delay))

(defun backoff (obj)
  (with-slots (limit max-delay) (the backoff-obj obj)
    (let ((delay (random limit)))
      (setf limit (min max-delay (* limit 2)))
      (sleep (/ delay 1000)))))
