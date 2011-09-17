(defpackage backoff-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :backoff-lock)

(defstruct lock-obj
  (flag nil :type boolean)
  (min-delay 0 :type fixnum)
  (max-delay 0 :type fixnum))

(defmacro test-and-set-true (flag)
  `(sb-ext:compare-and-swap ,flag nil t))
  

(defun make (&key (min-delay 4) (max-delay 128))
  (make-lock-obj :min-delay min-delay
                 :max-delay max-delay))

(defun lock (lock-obj)
  (let ((backoff (backoff:make (lock-obj-min-delay lock-obj)
                               (lock-obj-max-delay lock-obj))))
    (labels ((recur ()
               (if (lock-obj-flag lock-obj)
                   (recur)
                 (when (test-and-set-true (lock-obj-flag lock-obj))
                   (backoff:backoff backoff)
                   (recur)))))
      (recur))))

(defun unlock (lock-obj)
  (setf (lock-obj-flag lock-obj) nil))

(define-with-lock-macro with-lock lock unlock)

#|
(defun backoff-lock-test (lock-obj latch &aux (tid (thread-id:get)))
  (countdown-latch:countdown-and-await latch)
  
  (loop FOR i FROM 0 TO 10
    DO
    (backoff-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let* ((thread-num 5)
       (latch (countdown-latch:make thread-num))
       (lock (backoff-lock:make)))
  (loop REPEAT (1- thread-num)
        DO (sb-thread:make-thread 
            (lambda () 
              (backoff-lock-test lock latch))))
  (backoff-lock-test lock latch))
|#