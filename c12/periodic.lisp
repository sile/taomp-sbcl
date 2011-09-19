(defpackage periodic
  (:use :common-lisp :util)
  (:export make
           traverse))
(in-package :periodic)

(defstruct balancer 
  (toggle t :type boolean)
  (lock (@make-lock) :type @lock))

(defun balancer-traverse (balancer)
  (with-slots (toggle lock) (the balancer balancer)
    (@with-lock lock
      (prog1 (if toggle 0 1)
        (setf toggle (not toggle))))))


(defstruct layer
  (width 0 :type fixnum)
  (layer #() :type (vector balancer)))

(defun layer-new (width)
  (assert (zerop (nth-value 1 (round (log width 2))))) ; must be a power of 2
  (make-layer :width width
              :layer (loop REPEAT (/ width 2)
                       COLLECT (make-balancer) INTO list
                       FINALLY 
                       (return (coerce (append list (reverse list)) 'vector)))))
                           
(defun layer-traverse (@layer input)
  (with-slots (layer width) (the layer @layer)
    (let ((toggle (balancer-traverse (aref layer input))))
      (nth-value 
       toggle
       (if (< input (/ width 2))
           (values input #|lo|# (- width input 1) #|hi|#)
         (values (- width input 1) #|lo|# input #|hi|#))))))


(defstruct @block
  (north t :type (or null @block))
  (south t :type (or null @block))
  (layer t :type layer)
  (width 0 :type fixnum))

(defun @block-new (width)
  (assert (zerop (nth-value 1 (round (log width 2))))) ; must be a power of 2
  (make-@block :width width
               :layer (layer-new width)
               :north (when (> width 2)
                        (@block-new (/ width 2)))
               :south (when (> width 2)
                        (@block-new (/ width 2)))))

(defun @block-traverse (blk input)
  (with-slots (north south layer width) (the @block blk)
    (let ((wire (layer-traverse layer input))
          (half (/ width 2)))
      (cond ((<= width 2)
             wire)
            ((< wire half)
             (@block-traverse north wire))
            (t
             (+ half (@block-traverse south (- wire half))))))))

(defstruct periodic
  (@block '() :type list))

(defun make (width)
  (assert (zerop (nth-value 1 (round (log width 2))))) ; must be a power of 2
  (make-periodic :@block (loop REPEAT (log width 2)
                               COLLECT (@block-new width))))

(defun traverse (periodic input)
  (reduce (lambda (wire blk)
            (@block-traverse blk wire))
          (periodic-@block periodic)
          :initial-value input))
