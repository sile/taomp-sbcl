(defpackage unbounded-lockfree-stack
  (:use :common-lisp :util)
  (:shadow :common-lisp push pop)
  (:export make
           push
           pop))
(in-package :unbounded-lockfree-stack)

(defconstant +MIN_DELAY+ 2)
(defconstant +MAX_DELAY+ 128)

(defstruct node
  (value t :type t)
  (next nil :type (or null node)))

(defstruct stack
  (top nil :type (or null node)))

(defun make ()
  (make-stack))

(defun trypush (stack node)
  (let ((old-top (stack-top stack)))
    (setf (node-next node) old-top)
    (eq (sb-ext:compare-and-swap (stack-top stack) old-top node)
        old-top)))

(defun push (stack value)
  (loop WITH node = (make-node :value value)
        WITH bf = (backoff:make +MIN_DELAY+ +MAX_DELAY+)
    UNTIL (trypush stack node)
    DO
    (backoff:backoff bf)))

(defun trypop (stack)
  (let ((old-top (stack-top stack)))
    (when (null old-top)
      (error "EmptyException"))

    (let ((new-top (node-next old-top)))
      (if (eq (sb-ext:compare-and-swap (stack-top stack) old-top new-top)
              old-top)
          old-top
        nil))))

(defun pop (stack)
  (loop WITH bf = (backoff:make +MAX_DELAY+ +MAX_DELAY+)
        FOR return-node = (trypop stack)
    WHILE (null return-node)
    DO
    (backoff:backoff bf)
    FINALLY
    (return (node-value return-node))))
