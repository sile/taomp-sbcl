(defpackage simple-readwrite-lock 
  (:use :common-lisp :util)
  (:export make
           read-lock
           read-unlock
           write-lock
           write-unlock))
(in-package :simple-readwrite-lock)

(defstruct lock
  (readers   0 :type fixnum)
  (writer  nil :type boolean)
  (lock      t :type mutex-lock:lock)
  (cnd       t :type condition:object))

(defun make ()
  (let ((lock (mutex-lock:make)))
    (make-lock :lock lock
               :cnd (condition:make lock))))

(defun read-lock (obj)
  (with-slots (lock cnd writer readers) (the lock obj)
    (mutex-lock:with-lock (lock :re-entrant t)
      (while writer
        (condition:await cnd))
      (incf readers))))

(defun read-unlock (obj)
  (with-slots (lock cnd readers) (the lock obj)
    (mutex-lock:with-lock (lock :re-entrant t)
      (decf readers)
      (when (= 0 readers)
        (condition:broadcast cnd)))))

(defun write-lock (obj)
  (with-slots (lock cnd writer readers) (the lock obj)
    (mutex-lock:with-lock (lock :re-entrant t)
      (while (or (plusp readers)
                 writer)
        (condition:await cnd))
      (setf writer t))))

(defun write-unlock (obj)
  (with-slots (cnd writer) (the lock obj)  
    (setf writer nil)
    (condition:broadcast cnd)))
