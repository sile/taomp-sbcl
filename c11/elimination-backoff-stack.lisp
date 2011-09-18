(defpackage elimination-backoff-stack
  (:use :common-lisp :util)
  (:shadow :common-lisp push pop)
  (:export make 
           push
           pop))
(in-package :elimination-backoff-stack)

(defconstant +CAPACITY+ 10)

(defstruct (node (:include unbounded-lockfree-stack::node)))

(defstruct stack
  (elim-ary nil :type elimination-array:object)
  (range-policy nil :type tls-var)
  (impl nil :type unbounded-lockfree-stack::stack))

(defun make ()
  (make-stack :elim-ary (elimination-array:make +CAPACITY+)
              :range-policy (make-tls (lambda () (list +CAPACITY+)))
              :impl (unbounded-lockfree-stack:make)))

(defun range-get (range-policy)
  (car range-policy))

(defun range-policy-elimination-success (range-policy)
  (setf (car range-policy) +CAPACITY+))

(defun range-policy-elimination-timeout (range-plicy)
  (setf (car range-plicy) (max (round (car range-plicy) 2) 1)))
  
(defun push (stack value) 
  (with-slots (elim-ary range-policy impl) (the stack stack)
    (let ((range (tls-get range-policy))
          (node (make-node :value value)))
      (loop
       (when (unbounded-lockfree-stack::trypush impl node)
         (return))
       (handler-case
        (let ((other-value (elimination-array:visit
                            elim-ary value (range-get range))))
          (when (null other-value)
            (range-policy-elimination-success range)
            (return)))
        (error () ; timed out
         (range-policy-elimination-timeout range)))))))
            
(defun pop (stack)
  (with-slots (elim-ary range-policy impl) (the stack stack)
    (let ((range (tls-get range-policy)))
      (loop WITH return-node = (unbounded-lockfree-stack::trypop impl)
        DO
        (when return-node
          (return (node-value return-node)))

        (handler-case 
         (let ((other-value (elimination-array:visit
                             elim-ary nil (range-get range))))
           (when other-value
             (range-policy-elimination-success range)
             (return other-value)))
         (error ()
           (range-policy-elimination-timeout range)))))))
