(defpackage diffracting-tree 
  (:use :common-lisp :util)
  (:export make
           traverse))
(in-package :diffracting-tree)

(defconstant +DURATION+ 100)

(defstruct prism
  (exchanger #() :type (vector lockfree-exchanger:object)))

(defun prism-new (capacity)
  (make-prism
   :exchanger (coerce (loop REPEAT capacity
                            COLLECT (lockfree-exchanger:make))
                      'vector)))

(defun prism-visit (prism &aux (me (thread-id:get)))
  (let ((other (lockfree-exchanger:exchange
                (aref #1=(prism-exchanger prism)
                      (random (length #1#)))
                me
                +DURATION+)))
    (< me other)))

(defstruct balancer 
  (toggle t :type boolean)
  (lock (@make-lock) :type @lock))

(defun balancer-traverse (balancer)
  (with-slots (toggle lock) (the balancer balancer)
    (@with-lock lock
      (prog1 (if toggle 0 1)
        (setf toggle (not toggle))))))

(defstruct diffracting-balancer
  (prism t :type prism)
  (toggle t :type balancer))

(defun diffracting-balancer-new (capacity)
  (make-diffracting-balancer :prism (prism-new capacity)
                             :toggle (make-balancer)))

(defun diffracting-balancer-traverse (db) 
  (with-slots (prism toggle) (the diffracting-balancer db)
    (handler-case 
     (if (prism-visit prism)
         0
       1)
     (error () ; timed-out
       (balancer-traverse toggle)))))


(defstruct diffracting-tree
  (size 0 :type fixnum)
  (root t :type diffracting-balancer)
  (child #() :type (or null (vector diffracting-tree))))

(defun make (size)
  (make-diffracting-tree
   :size size
   :root (diffracting-balancer-new size)
   :child (when (> size 2)
            `#(,(make (/ size 2))
               ,(make (/ size 2))))))

(defun traverse (tree)
  (with-slots (root child size) (the diffracting-tree tree)
    (let ((half (diffracting-balancer-traverse root)))
      (if (<= size 2)
          half
        (+ (* 2 (traverse (aref child half))) half)))))
