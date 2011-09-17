(defpackage countdown-latch 
  (:use :common-lisp)
  (:export make
           countdown-and-await))
(in-package :countdown-latch)

(defstruct latch 
  (count 0 :type fixnum))

(defun make (count)
  (make-latch :count (max 0 count)))

(defmacro barrier (exp)
  `(sb-thread:barrier (:data-dependency) ,exp))

(defun atomic-decriment-if-plus (latch)
  (let* ((old (latch-count latch))
         (new (1- old)))
    (when (plusp old)
      (unless (eq old (sb-ext:compare-and-swap (latch-count latch) old new))
        (atomic-decriment-if-plus latch)))))

(defun countdown-and-await (latch)
  (atomic-decriment-if-plus latch)
  
  (loop FOR cnt = (barrier (latch-count latch))
        WHILE (plusp cnt)
    DO
    (sleep (/ (- cnt 1) 1000))))

