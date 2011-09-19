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
                :sub (when (> width 2) (merger-new (/ width 2)))
                :half (when (> width 2)
                        (list (make (floor width 2))
                              (make (floor width 2))))))

(defun traverse (bitonic input &aux (output 0))
  (with-slots (half merger width) (the bitonic bitonic)
#|
    (when half
      (setf output (traverse (first half) (floor input 2))))
    (print (list (floor input 2) output))
    (merger-traverse merger output)
|#

    (when (> width 2)
      (setf output
            (traverse (nth (floor input (floor width 2)) half)
                      (floor input 2))))

    (when (> width 2)
      (print output))

    (+ (merger-traverse merger (if (>= input (floor width 2))
                                   (floor width 2)
                                 0))
         output)))
