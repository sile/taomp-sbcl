(defpackage filter-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :filter-lock)

(defconstant true 1)
(defconstant false 0)

(defstruct lock-obj
  (max-thread-num 10 :type fixnum)
  (level #());(make-array max-thread-num :initial-element 0))
  (victim #()));(make-array max-thread-num :initial-element 0)))

(defun make (&optional (max-thread-num 10))
  (make-lock-obj :max-thread-num max-thread-num
                 :level (make-array max-thread-num :initial-element 0)
                 :victim (make-array max-thread-num :initial-element 0)))

(defun lock (lock-obj)
  (with-slots (level victim max-thread-num) (the lock-obj lock-obj)
    (let ((me (thread-id:get)))
      (loop FOR cur-level FROM 1 BELOW max-thread-num 
        DO
        (setf (aref level me) cur-level
              (aref victim cur-level) me)
        ;; spin while conflicts exist
        (while (and (loop FOR other-level ACROSS level
                          FOR other FROM 0 
                          THEREIS (and (not (= me other))
                                       (>= other-level cur-level)))
                    (= (aref victim cur-level) me)))))))

(defun unlock (lock-obj)
  (with-slots (level) (the lock-obj lock-obj)
    (let ((me (thread-id:get)))
      (setf (aref level me) 0))))

(define-with-lock-macro with-lock lock unlock)

#|
(defun filter-lock-test (lock-obj latch &aux (tid (thread-id:get)))
  (countdown-latch:countdown-and-await latch)
  
  (loop FOR i FROM 0 TO 10
    DO
    (filter-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let* ((thread-num 5)
       (latch (countdown-latch:make thread-num))
       (lock (filter-lock:make thread-num)))
  (loop REPEAT (1- thread-num)
        DO (sb-thread:make-thread 
            (lambda () 
              (filter-lock-test lock latch))))
  (filter-lock-test lock latch))
|#