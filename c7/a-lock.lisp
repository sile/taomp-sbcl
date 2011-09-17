(defpackage a-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :a-lock)

(defstruct lock-obj
  (tail 0 :type atomic-uint)
  (flag #() :type (simple-array boolean))
  (my-slot-index (make-tls) :type tls-var))
  

(defun make (&optional (capacity 128))
  (let ((flag (make-array capacity :element-type 'boolean :initial-element nil)))
    (setf (aref flag 0) t)
    (make-lock-obj :flag flag)))

(defun lock (lock-obj)
  (with-slots (tail flag my-slot-index) (the lock-obj lock-obj)
    (let ((slot (mod (sb-ext:atomic-incf (lock-obj-tail lock-obj)) (length flag))))
      (setf (tls-get my-slot-index) slot)
      (while (not (aref flag slot))))))

(defun unlock (lock-obj)
  (with-slots (tail flag my-slot-index) (the lock-obj lock-obj)
    (let ((slot (tls-get my-slot-index)))
      (setf (aref flag slot) nil
            (aref flag (mod (1+ slot) (length flag))) t))))

(define-with-lock-macro with-lock lock unlock)

#|
(defun a-lock-test (lock-obj latch &aux (tid (thread-id:get)))
  (countdown-latch:countdown-and-await latch)
  
  (loop FOR i FROM 0 TO 10
    DO
    (a-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let* ((thread-num 5)
       (latch (countdown-latch:make thread-num))
       (lock (a-lock:make)))
  (loop REPEAT (1- thread-num)
        DO (sb-thread:make-thread 
            (lambda () 
              (a-lock-test lock latch))))
  (a-lock-test lock latch))
|#