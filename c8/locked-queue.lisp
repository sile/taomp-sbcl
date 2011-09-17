(defpackage locked-queue
  (:use :common-lisp :util)
  (:export make
           enq
           deq

           queue))
(in-package :locked-queue)

(defstruct queue
  (lock t :type mutex-lock:lock)
  (not-full t :type condition:object)
  (not-empty t :type condition:object)
  (items #() :type vector)
  (tail 0 :type fixnum)
  (head 0 :type fixnum)
  (count 0 :type fixnum))

(defun make (capacity)
  (let ((lock (mutex-lock:make)))
    (make-queue :lock lock
                :not-full (condition:make lock)
                :not-empty (condition:make lock)
                :items (make-array capacity))))

(defun enq (que x)
  (with-slots (lock not-full not-empty count tail items) (the queue que)
    (mutex-lock:with-lock (lock :re-entrant t)
      (while (= count (length items))
        (condition:await not-full))
      
      (setf (aref items tail) x)
      (when (= (incf tail) (length items))
        (setf tail 0))
      (incf count)
      (condition:notify not-empty)))
  x)

(defun deq (que) 
  (with-slots (lock not-full not-empty count head items) (the queue que)
    (mutex-lock:with-lock (lock :re-entrant t)
      (while (= count 0)
        (condition:await not-empty))
      
      (prog1 (aref items head)
        (when (= (incf head) (length items))
          (setf head 0))
        (decf count)
        (condition:notify not-full)))))

