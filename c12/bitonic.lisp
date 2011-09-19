(defpackage bitonic
  (:use :common-lisp :util)
  (:export make
           traverse))
(in-package :bitonic)

(defstruct balancer 
  (toggle t :type boolean)
  (lock (@make-lock) :type @lock))

(defun balancer-traverse (balancer)
  (with-slots (toggle lock) (the balancer balancer)
    (@with-lock lock
      (prog1 (if toggle 0 1)
        (setf toggle (not toggle))))))


(defstruct merger
  (half  '() :type list)  ; two half-width merge networks
  (layer #() :type (vector balancer)) ; final layer
  (width 0 :type fixnum))

(defun merger-new (width) 
  (assert (zerop (mod width 2))) ; must be a power of 2
  (make-merger :width width
               :layer (loop REPEAT (/ width 2)
                            COLLECT (make-balancer) INTO list
                            FINALLY (return (coerce list 'vector)))
               :half (when (> width 2)
                       (list (merger-new (/ width 2))
                             (merger-new (/ width 2))))))

(defun merger-traverse (merger input)
  (with-slots (half layer width) (the merger merger)
    (let* ((i (if (< input (/ width 2))
                  (mod input 2)
                (- 1 (mod input 2))))
           (output (if (null half)
                       0
                     (merger-traverse (nth i half) (floor input 2)))))
      (+ (* 2 output) 
         (balancer-traverse (aref layer output))))))


(defstruct bitonic 
  (half '() :type list) ; two half-width bitonic networks
  (merger nil :type merger) ; final merger 
  (width 0 :type fixnum))

(defun make (width)
  (assert (zerop (mod width 2))) ; must be a power of 2
  (make-bitonic :width width
                :merger (merger-new width)
                :half (when (> width 2)
                        (list (make (floor width 2))
                              (make (floor width 2))))))

(defun traverse (bitonic input)
  (with-slots (half merger width) (the bitonic bitonic)
    (cond ((<= width 2)
           (merger-traverse merger input))
          ((< input (floor width 2))
           (merger-traverse merger (traverse (first half) (floor input 2))))
          (t
           (merger-traverse merger (+ (floor width 2)
                                      (traverse (second half) (floor input 2))))))))
