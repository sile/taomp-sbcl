(defpackage composit-lock
  (:use :common-lisp :util)
  (:export make
           trylock
           unlock
           with-lock))
(in-package :composit-lock)

(defconstant +SIZE+ 8)
(defconstant +MIN_BACKOFF+ 4)
(defconstant +MAX_BACKOFF+ 128)

(deftype state () '(member :free :waiting :released :aborted))

(defstruct node
  (state :free :type state)
  (pred nil :type (or null node)))

(defstruct lock-obj
  (tail (asr:make nil 0) :type asr:asr)
  (waiting #() :type (vector node))
  (my-node (make-tls #'make-node) :type tls-var))

(defun make ()
  (let ((waiting (make-array +SIZE+ :element-type 'node
                                    :initial-element (make-node))))
    (dotimes (i +SIZE+)
      (setf (aref waiting i) (make-node)))
    (make-lock-obj :waiting waiting)))

(declaim (ftype (function (t t t) t) acquire-node splice-node)
         (ftype (function (t t t t) t) wait-for-predecessor))

(defun trylock (obj timeout)
  (let ((tm (timeout:make timeout))
        (backoff (backoff:make +MIN_BACKOFF+ +MAX_BACKOFF+)))
    (handler-case
     (progn
       (let* ((node (acquire-node obj backoff tm))
              (pred (splice-node obj node tm)))
         (wait-for-predecessor obj pred node tm))
       t)
     (error   ; TODO: catch only timeout-exception
      nil))))
    
(defun unlock (obj)
  (let ((acq-node (tls-get (lock-obj-my-node obj))))
    (setf (node-state acq-node) :released
          (tls-get (lock-obj-my-node obj)) nil)))

(defun acquire-node (obj backoff tm)
  (with-slots (waiting tail) (the lock-obj obj)
    (let ((node (aref waiting (random +SIZE+))))
      (loop
       (when (eq :free (sb-ext:compare-and-swap (node-state node) :free :waiting))
         (return-from acquire-node node))
       
       (multiple-value-bind (cur-tail cur-stamp) (asr:get tail)
         (let ((state (node-state node)))
           (when (or (eq state :aborted)
                     (eq state :released))
             (when (eq node cur-tail)
               (let ((my-pred (if (not (eq state :aborted))
                                  nil
                                (node-pred node))))
                 (when (asr:compare-and-set tail
                         cur-tail my-pred cur-stamp (1+ cur-stamp))
                   (setf (node-state node) :waiting)
                   (return-from acquire-node node)))))))
       (backoff:backoff backoff)
       (when (timeout:expired? tm)
         (error "Timed-out"))))))
      
(defun splice-node (obj node tm)
  (with-slots (tail) (the lock-obj obj)
    (loop
     (multiple-value-bind (cur-tail cur-stamp) (asr:get tail)
       (when (timeout:expired? tm)
         (setf (node-state node) :free)
         (print 2)
         (error "Timed-out"))
       
       (when (asr:compare-and-set tail cur-tail node cur-stamp (1+ cur-stamp))
         (return cur-tail))))))

(defun wait-for-predecessor (obj pred node tm)
  (with-slots (my-node) (the lock-obj obj)
    (when (null pred)
      (setf (tls-get my-node) node)
      (return-from wait-for-predecessor))

    (loop FOR pred-state = (node-state pred)
          UNTIL (eq pred-state :released)
      DO
      (when (eq pred-state :aborted) 
        (setf (node-state pred) :free
              pred (node-pred pred)))

      (when (timeout:expired? tm)
        (setf (node-pred node) pred
              (node-state node) :aborted)
        (error "Timed-out")))

    (setf (node-state pred) :free
          (tls-get my-node) node)))

(define-with-trylock-macro with-trylock trylock unlock)
