(defpackage fifo-readwrite-lock
  (:use :common-lisp :util)
  (:export make
           read-lock
           read-unlock
           write-lock
           write-unlock))
(in-package :fifo-readwrite-lock)

(defstruct lock
  (read-acquires 0 :type fixnum)
  (read-releases 0 :type fixnum)
  (writer nil :type boolean)
  (lock t :type mutex-lock:lock)
  (cnd t :type condition:object))

(defun make ()
  (let ((lock (mutex-lock:make)))
    (make-lock :lock lock
               :cnd (condition:make lock))))

(defun read-lock (obj)
  (with-slots (lock cnd writer read-acquires) (the lock obj)
    (mutex-lock:with-lock (lock :re-entrant t)
      (while writer
        (condition:await cnd))
      (incf read-acquires))))

(defun read-unlock (obj)
  (with-slots (lock cnd read-acquires read-releases) (the lock obj)
    (mutex-lock:with-lock (lock :re-entrant t)
      (incf read-releases)
      (when (= read-acquires read-releases)
        (condition:broadcast cnd)))))

(defun write-lock (obj)
  (with-slots (lock cnd writer read-acquires read-releases) (the lock obj)
    (mutex-lock:with-lock (lock :re-entrant t)
      (while writer
        (condition:await cnd))
      (setf writer t)
      (while (/= read-acquires read-releases)
        (condition:await cnd)))))

(defun write-unlock (obj)
  (with-slots (cnd writer) (the lock obj)
    (setf writer nil)
    (condition:broadcast cnd)))

      
      
  
