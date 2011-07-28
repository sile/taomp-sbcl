(defpackage lock-one
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :lock-one)

(defconstant true 1)
(defconstant false 0)

(defstruct lock-obj
  (flag (make-array 2 :element-type 'bit :initial-element false)
        :type (simple-bit-vector 2)))

(defun make ()
  (make-lock-obj))

(defun lock (lock-obj)
  (with-slots (flag) (the lock-obj lock-obj)
    (let* ((me (thread-id:get))
           (other (- 1 me)))
      (setf (bit flag me) true)
      (while (= true (bit flag other))))))

(defun unlock (lock-obj)
  (with-slots (flag) (the lock-obj lock-obj)
    (let ((me (thread-id:get)))
      (setf (bit flag me) false))))

(define-with-lock-macro with-lock lock unlock)


#|
(defun lock-one-test (lock-obj)
  (loop WITH tid = (thread-id:get)
        FOR i FROM 0 TO 10
    DO
    (lock-one:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let ((lock (lock-one:make)))
  (sb-thread:make-thread 
   (lambda () 
     (lock-one-test lock)))
  (lock-one-test lock))
|#