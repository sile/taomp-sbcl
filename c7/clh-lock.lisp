(defpackage clh-lock
  (:use :common-lisp :util)
  (:export make
           lock
           unlock
           with-lock))
(in-package :clh-lock)

(defstruct node
  (locked nil :type boolean))

(defstruct lock-obj
  (tail nil :type node)
  (my-pred (make-tls) :type tls-var)
  (my-node (make-tls #'make-node) :type tls-var))

(defun make ()
  (make-lock-obj :tail (make-node)))

(defun lock (obj)
  (with-slots (my-pred my-node) (the lock-obj obj)
    (let ((node (tls-get my-node)))
      (setf (node-locked node) t)
      (let ((pred (get-and-set (lock-obj-tail obj) node)))
        (setf (tls-get my-pred) pred)

        (while (node-locked pred))))))

(defun unlock (obj)
  (with-slots (my-pred my-node) (the lock-obj obj)
    (let ((node (tls-get my-node)))
      (setf (node-locked node) nil
            (tls-get my-node) (tls-get my-pred)))))

(define-with-lock-macro with-lock lock unlock)

