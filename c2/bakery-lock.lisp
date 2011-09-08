(defpackage bakery-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :bakery-lock)


(defstruct lock-obj
  (max-thread-num 10 :type fixnum)
  (flag #() :type (vector boolean))
  (label #() :type (vector fixnum)))

(defun make (&optional (max-thread-num 10))
  (make-lock-obj :max-thread-num max-thread-num
                 :flag (make-array max-thread-num
                                   :element-type 'boolean 
                                   :initial-element nil)
                 :label (make-array max-thread-num
                                    :element-type 'fixnum
                                    :initial-element 0)))

(defun lock (lock-obj)
  (with-slots (flag label max-thread-num) (the lock-obj lock-obj)
    (let ((me (thread-id:get)))
      (setf (aref flag me) t
            (aref label me) (1+ (loop FOR x ACROSS label MAXIMIZE x)))
      (loop WHILE (loop FOR other FROM 0 BELOW max-thread-num
                    THEREIS (and (/= other me)
                                 (aref flag other)
                                 (or (< (aref label other) (aref label me))
                                     (and (= (aref label other) (aref label me))
                                          (< other me)))))))))

(defun unlock (lock-obj)
  (with-slots (flag) (the lock-obj lock-obj)  
    (setf (aref flag (thread-id:get)) nil)))

(define-with-lock-macro with-lock lock unlock)

#|
(defun bakery-lock-test (lock-obj latch &aux (tid (thread-id:get)))
  (countdown-latch:countdown-and-await latch)
  
  (loop FOR i FROM 0 TO 10
    DO
    (bakery-lock:with-lock lock-obj
      (format t "~&; <THREAD ~a> ~a~%" tid i)))
  'done)

(let* ((thread-num 5)
       (latch (countdown-latch:make thread-num))
       (lock (bakery-lock:make thread-num)))
  (loop REPEAT (1- thread-num)
        DO (sb-thread:make-thread 
            (lambda () 
              (bakery-lock-test lock latch))))
  (bakery-lock-test lock latch))
|#