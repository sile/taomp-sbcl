(defpackage lock-two
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :lock-two)

(defstruct lock-obj
  victime)

(defun make ()
  (make-lock-obj))

(defun lock (lock-obj)
  (with-slots (victime) (the lock-obj lock-obj)
    (let ((me (thread-id:get)))
      (setf victime me)         ; let the other go first
      (while (= victime me))))) ; wait

(defun unlock (lock-obj)
  (declare (ignore lock-obj)))

(define-with-lock-macro with-lock lock unlock)

#|
(defun lock-two-test (lock-obj)
  (loop WITH tid = (thread-id:get)
        FOR i FROM 0 TO 10
    DO
    (lock-two:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let ((lock (lock-two:make)))
  (sb-thread:make-thread 
   (lambda () 
     (lock-two-test lock)))
  (lock-two-test lock))
|#