(defpackage tsa-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :tsa-lock)

(defstruct lock-obj
  (flag nil :type boolean))

(defmacro test-and-set-true (flag)
  `(sb-ext:compare-and-swap ,flag nil t))
  

(defun make ()
  (make-lock-obj))

(defun lock (lock-obj)
  (when (test-and-set-true (lock-obj-flag lock-obj))
    (lock lock-obj)))

(defun unlock (lock-obj)
  (setf (lock-obj-flag lock-obj) nil))

(define-with-lock-macro with-lock lock unlock)

#|
(defun tsa-lock-test (lock-obj latch &aux (tid (thread-id:get)))
  (countdown-latch:countdown-and-await latch)
  
  (loop FOR i FROM 0 TO 10
    DO
    (tsa-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let* ((thread-num 5)
       (latch (countdown-latch:make thread-num))
       (lock (tsa-lock:make)))
  (loop REPEAT (1- thread-num)
        DO (sb-thread:make-thread 
            (lambda () 
              (tsa-lock-test lock latch))))
  (tsa-lock-test lock latch))
|#