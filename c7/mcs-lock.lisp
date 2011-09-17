(defpackage mcs-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :mcs-lock)

(defstruct node
  (locked nil :type boolean)
  (next nil :type (or null node)))

(defstruct lock-obj
  (tail nil :type (or null node))
  (my-node (make-tls #'make-node) :type tls-var))

(defun make ()
  (make-lock-obj))

(defun lock (obj)
  (let* ((node (tls-get (lock-obj-my-node obj)))
         (pred (get-and-set (lock-obj-tail obj) node)))
    (when pred
      (setf (node-locked node) t
            (node-next pred) node)
      
      ;; wait until predecessor gives up the lock
      (while (node-locked node)))))

(defun unlock (obj)
  (let ((node (tls-get (lock-obj-my-node obj))))
    (when (null (node-next node))
      (when (eq node (sb-ext:compare-and-swap (lock-obj-tail obj) node nil))
        (return-from unlock))
      ;; wait until successor fills in the next field
      (while (null (node-next node))))

    (setf (node-locked (node-next node)) nil
          (node-next node) nil)))

(define-with-lock-macro with-lock lock unlock)

