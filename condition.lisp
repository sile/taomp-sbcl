(defpackage condition
  (:use :common-lisp)
  (:export make
           await
           notify
           broadcast

           object))
(in-package :condition)

(defstruct object
  (wq (sb-thread:make-waitqueue) :type sb-thread:waitqueue)
  (lock (mutex-lock:make) :type mutex-lock:lock))

(defmethod print-object ((o object) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "~a ~s ~s" :condition :lock (object-lock o))))

(defun make (lock)
  (make-object :lock lock))

(defun await (obj)
  (with-slots (lock wq) (the object obj)
    (sb-thread:condition-wait wq (mutex-lock::lock-obj-mutex lock))))

(defun notify (obj)
  (sb-thread:condition-notify (object-wq obj)))

(defun broadcast (obj)
  (sb-thread:condition-broadcast (object-wq obj)))
