(defpackage simple-reentrant-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :simple-reentrant-lock)

(defstruct lock
  (lock t :type mutex-lock:lock)
  (cnd t :type condition:object)
  (owner t :type t)
  (hold-count 0 :type fixnum))

(defun make ()
  (let ((lock (mutex-lock:make)))
    (make-lock :lock lock
               :cnd (condition:make lock))))

(defun lock (obj &aux (me sb-thread:*current-thread*))
  (with-slots (lock cnd owner hold-count) (the lock obj)
    (mutex-lock:with-lock (lock)
      (when (eq owner me)
        (incf hold-count)
        (return-from lock))

      (while (plusp hold-count)
        (condition:await cnd))

      (setf owner me
            hold-count 1))))

(defun unlock (obj &aux (me sb-thread:*current-thread*))
  (with-slots (lock cnd owner hold-count) (the lock obj)
    (mutex-lock:with-lock (lock)
      (when (or (= 0 hold-count)
                (not (eq owner me)))
        (error "illegal-monitor-state-exception"))
      
      (decf hold-count)
      (when (= hold-count 0)
        (condition:notify cnd)))))

(define-with-lock-macro with-lock lock unlock)
