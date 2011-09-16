(defpackage ttsa-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :ttsa-lock)

(defstruct lock-obj
  (flag nil :type boolean))

(defmacro test-and-set-true (flag)
  `(sb-ext:compare-and-swap ,flag nil t))
  

(defun make ()
  (make-lock-obj))

(defun lock (lock-obj)
  (when (or (lock-obj-flag lock-obj)
            (test-and-set-true (lock-obj-flag lock-obj)))
    (lock lock-obj)))

(defun unlock (lock-obj)
  (setf (lock-obj-flag lock-obj) nil))

(define-with-lock-macro with-lock lock unlock)

#|
(defun ttsa-lock-test (lock-obj latch &aux (tid (thread-id:get)))
  (countdown-latch:countdown-and-await latch)
  
  (loop FOR i FROM 0 TO 10
    DO
    (ttsa-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let* ((thread-num 5)
       (latch (countdown-latch:make thread-num))
       (lock (ttsa-lock:make)))
  (loop REPEAT (1- thread-num)
        DO (sb-thread:make-thread 
            (lambda () 
              (ttsa-lock-test lock latch))))
  (ttsa-lock-test lock latch))
|#