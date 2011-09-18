(defpackage bounded-queue
  (:use :common-lisp :util)
  (:export make
           enq
           deq))
(in-package :bounded-queue)

(defstruct node
  (value t :type t)
  (next nil :type (or null node)))

(defstruct queue 
  (enq-lock t :type @lock)
  (deq-lock t :type @lock)
  (not-empty t :type condition:object)
  (not-full t :type condition:object)
  (size 0 :type atomic-uint)
  (capacity 0 :type fixnum)
  (head nil :type node)
  (tail nil :type node))

(defun make (capacity)
  (let ((enq (@make-lock))
        (deq (@make-lock))
        (node (make-node :value nil)))
    (make-queue :capacity capacity
                :enq-lock enq
                :deq-lock deq
                :not-empty (condition:make deq)
                :not-full (condition:make enq)
                :head node
                :tail node)))

(defun enq (que x &aux must-wake-dequeuers)
  (with-slots (enq-lock deq-lock size capacity not-full not-empty tail)
              (the queue que)
    (@with-lock enq-lock
      (while (= size capacity)
        (condition:await not-full))
      
      (let ((node (make-node :value x)))
        (setf (node-next tail) node
              tail node))

      (when (= 0 (sb-ext:atomic-incf (queue-size que)))
        (setf must-wake-dequeuers t)))

    (@with-lock deq-lock
      (condition:broadcast not-empty))))

(defun deq (que &aux must-wake-enqueuers)
  (with-slots (enq-lock deq-lock size capacity not-full not-empty head)
              (the queue que)
    (prog1
        (@with-lock deq-lock
          (while (= 0 size)
            (condition:await not-empty))

          (let ((result (node-value (node-next head))))
            (setf head (node-next head))
            
            (when (= (sb-ext:atomic-decf (queue-size que)) capacity)
              (setf must-wake-enqueuers t))

            result))
      (@with-lock enq-lock
        (condition:broadcast not-full)))))


        