(defpackage lock-one
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :lock-one)

(defconstant true 1)
(defconstant false 0)

(defstruct lock
  (flag (make-array 2 :element-type 'bit :initial-element false)
        :type (simple-bit-vector 2)))

(defun make ()
  (make-lock))

(defun lock (lock-obj)
  (with-slots (flag) (lock lock-obj)
    (let* ((me (thread-id:get))
           (other (- 1 me)))
      (setf (bit flag me) true)
      (while (= true (bit flag other))))))

(defun unlock (lock-obj)
  (with-slots (flag) (lock lock-obj)
    (let ((me (thread-id:get)))
      (setf (bit flag me) false))))

(defmacro with-lock (lock-obj &body body)
  (let ((obj (gensym)))
    `(let ((,obj ,lock-obj))
       (lock ,obj)
       (unwind-protect
           (locally ,@body)
         (unlock ,obj)))))
