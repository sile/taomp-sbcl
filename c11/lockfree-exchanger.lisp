(defpackage lockfree-exchanger
  (:use :common-lisp :util)
  (:export make
           exchange))
(in-package :lockfree-exchanger)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +EMPTY+ 0)
  (defconstant +BUSY+ 1)
  (defconstant +WAITING+ 2))

(defstruct object
  (slot (asr:make nil +EMPTY+) :type asr:asr))

(defun make ()
  (make-object))

(defun exchange (exchanger my-item timeout &aux (tm (timeout:make timeout)))
  (loop
   (when (timeout:expired? tm)
     (error "Timed out"))

   (multiple-value-bind (your-item stamp) 
                        (asr:get #1=(object-slot exchanger))
     (ecase stamp
       (#.+EMPTY+ 
        (when (asr:compare-and-set #1# your-item my-item +EMPTY+ +WAITING+)
          (while (not (timeout:expired? tm))
            (multiple-value-bind (your-item stamp) (asr:get #1#)
              (when (= stamp +BUSY+)
                (asr:set #1# nil +EMPTY+)
                (return your-item))))
          (if (asr:compare-and-set #1# my-item nil +WAITING+ +EMPTY+)
              (error "Timed out")
            (return (prog1 (asr:get #1#)
                      (asr:set #1# nil +EMPTY+))))))
       (#.+WAITING+ 
        (when (asr:compare-and-set #1# your-item my-item +WAITING+ +BUSY+)
          (return your-item)))
       (#.+BUSY+)))))
