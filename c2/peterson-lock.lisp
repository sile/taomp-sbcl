(defpackage peterson-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :peterson-lock)

(defconstant true 1)
(defconstant false 0)

(defstruct lock-obj
  (flag (make-array 2 :element-type 'bit :initial-element false)
        :type (simple-bit-vector 2))
  victime)

(defun make ()
  (make-lock-obj))

(defun lock (lock-obj)
  (with-slots (flag victime) (the lock-obj lock-obj)
    (let* ((me (thread-id:get))
           (other (- 1 me)))
      (setf (bit flag me) true)
      (setf victime me)         ; let the other go first
      (while (and (= true (bit flag other))
                  (= victime me))))))

(defun unlock (lock-obj)
  (with-slots (flag) (the lock-obj lock-obj)
    (let ((me (thread-id:get)))
      (setf (bit flag me) false))))

(define-with-lock-macro with-lock lock unlock)

#|
(defun peterson-lock-test (lock-obj)
  (loop WITH tid = (thread-id:get)
        FOR i FROM 0 TO 10
    DO
    (peterson-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let ((lock (peterson-lock:make)))
  (sb-thread:make-thread 
   (lambda () 
     (peterson-lock-test lock)))
  (peterson-lock-test lock))
|#