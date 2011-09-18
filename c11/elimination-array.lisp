(defpackage elimination-array
  (:use :common-lisp :util)
  (:export make
           visit

           object))
(in-package :elimination-array)

(defconstant +DURATION+ 10)

(defstruct object
  (exchanger #() :type (vector lockfree-exchanger:object)))

(defun make (capacity)
  (make-object 
   :exchanger (coerce (loop REPEAT capacity
                            COLLECT (lockfree-exchanger:make))
                      'vector)))

(defun visit (elim-ary value range) 
  (lockfree-exchanger:exchange
   (aref (object-exchanger elim-ary)
         (random range))
   value
   +DURATION+))

