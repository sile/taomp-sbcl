(defpackage unbounded-lockfree-queue
  (:use :common-lisp :util)
  (:export make
           enq
           deq))
(in-package :unbounded-lockfree-queue)

(defstruct node
  (value t :type t)
  (next nil :type (or null node)))

(defstruct queue 
  (head nil :type node)
  (tail nil :type node))

(defun make ()
  (let ((node (make-node :value nil)))
    (make-queue :head node
                :tail node)))

(defun enq (que x &aux (node (make-node :value x)))
  (loop FOR last = (queue-tail que)
        FOR next = (node-next last)
    DO
    (when (eq last (queue-tail que))
      (if next
          (sb-ext:compare-and-swap (queue-tail que) last next)
        (when (eq (sb-ext:compare-and-swap (node-next last) nil node) nil)
          (sb-ext:compare-and-swap (queue-tail que) last node)
          (return))))))

(defun deq (que)
  (loop FOR first = (queue-head que)
        FOR last = (queue-tail que)
        FOR next = (node-next first)
    DO
    (when (eq first (queue-head que))
      (if (eq first last)
          (if (null next)
              (error "EmptyException")
            (sb-ext:compare-and-swap (queue-tail que) last next))
        (when (eq (sb-ext:compare-and-swap (queue-head que) first next)
                  first)
          (return (node-value next)))))))

          
        