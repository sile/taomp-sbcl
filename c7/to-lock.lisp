(defpackage to-lock
  (:use :common-lisp :util)
  (:export make
           trylock
           unlock
           with-lock))
(in-package :to-lock)

(defstruct node
  (pred nil :type (or null node)))

(defvar *available* (make-node))

(defstruct lock-obj
  (tail nil :type (or null node))
  (my-node (make-tls #'make-node) :type tls-var))

(defun make ()
  (make-lock-obj))

(defun trylock (obj timeout)
  (let ((tm (timeout:make timeout))
        (node (make-node)))
    (setf (tls-get (lock-obj-my-node obj)) node)
    
    (let ((my-pred (get-and-set (lock-obj-tail obj) node)))
      (when (or (null my-pred)
                (eq (node-pred my-pred) *available*))
        (return-from trylock t))

      (while (not (timeout:expired? tm))
        (let ((pred-pred (node-pred my-pred)))
          (when (eq pred-pred *available*)
            (return-from trylock t))
          (when pred-pred
            (setf my-pred pred-pred))))

      (unless (eq node (sb-ext:compare-and-swap (lock-obj-tail obj) node my-pred))
        (setf (node-pred node) my-pred))))
  nil)
            
(defun unlock (obj)
  (let ((node (tls-get (lock-obj-my-node obj))))
    (unless (eq node (sb-ext:compare-and-swap (lock-obj-tail obj) node nil))
      (setf (node-pred node) *available*))))

(define-with-trylock-macro with-trylock trylock unlock)

