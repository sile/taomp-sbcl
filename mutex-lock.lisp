(defpackage mutex-lock
  (:use :common-lisp)
  (:export make
           lock
           unlock
           with-lock))
(in-package :mutex-lock)

(defstruct lock-obj
  (mutex (sb-thread:make-mutex) :type sb-thread:mutex))

(defmethod print-object ((o lock-obj) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "~a" :mutex-lock)))

(defun make ()
  (make-lock-obj))
  
(defun lock (obj)
  (sb-thread:get-mutex (lock-obj-mutex obj)))

(defun unlock (obj)
  (sb-thread:release-mutex (lock-obj-mutex obj)))

(defmacro with-lock ((obj &key reentranct) &body body)
  (if reentranct
      `(sb-thread:with-recursive-lock (lock-obj-mutex ,obj)
         ,@body)
    `(sb-thread:with-mutex (lock-obj-mutex ,obj)
       ,@body)))
