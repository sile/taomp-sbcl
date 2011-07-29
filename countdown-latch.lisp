(defpackage countdown-latch 
  (:use :common-lisp)
  (:export make
           countdown-and-await))
(in-package :countdown-latch)

(deftype atomic-int ()
  #+x86-64 '(unsigned-byte 64)
  #+x86 '(unsigned-byte 32))

(defstruct latch 
  (count 0 :type atomic-int))

(defun make (count)
  (make-latch :count count))

(defmacro barrier (exp)
  `(sb-thread:barrier (:data-dependency) ,exp))

(defun countdown-and-await (latch)
  (let ((old (latch-count latch)))
    (sb-ext:atomic-decf (latch-count latch))
    (loop FOR cur = (barrier (latch-count latch))
          WHILE (and (< 0 cur old)))))
