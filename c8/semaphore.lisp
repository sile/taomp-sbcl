(defpackage semaphore
  (:use :common-lisp :util)
  (:export make
           acquire
           release))
(in-package :semaphore)

(defstruct semaphore
  (capacity 0 :type fixnum)
  (state 0 :type fixnum)
  (lock t :type mutex-lock:lock)
  (cnd t :type condition:object))

(defun make (capacity)
  (let ((lock (mutex-lock:make)))
    (make-semaphore :capacity capacity
                    :lock lock
                    :cnd (condition:make lock))))

(defun acquire (obj)
  (with-slots (lock cnd state capacity) (the semaphore obj)
    (mutex-lock:with-lock (lock)
      (while (= state capacity)
        (condition:await cnd))
      (incf state))))

(defun release (obj)
  (with-slots (lock cnd state) (the semaphore obj)
    (mutex-lock:with-lock (lock)
      (decf state)
      (condition:broadcast cnd))))
