(defpackage atomic-markable-reference
  (:use :common-lisp)
  (:shadow :common-lisp get set)
  (:nicknames amr)
  (:export make
           get
           compare-and-set

           object))
(in-package :atomic-markable-reference)

(defstruct ref
  (x t :type t)
  (mark nil :type boolean))

(defstruct object
  (ref nil :type ref))

(defmethod print-object ((o object) stream)
  (print-unreadable-object (o stream)
    (format stream "~a ~s ~s ~s ~s" 
            :amr :value (ref-x (object-ref o)) :mark (ref-mark (object-ref o)))))
  
(defun make (val initial-mark)
  (make-object :ref (make-ref :x val :mark initial-mark)))

(defun get (obj)
  (let ((o (object-ref obj)))
    (values (ref-x o) (ref-mark o))))

(defmacro compare-and-set (obj expected new expected-mark new-mark)
  `(let ((ref (object-ref ,obj)))
     (and (eq (ref-mark ref) ,expected-mark)
          (eq (ref-x ref) ,expected)
          (eq (sb-ext:compare-and-swap (object-ref ,obj)
                                       ref
                                       (make-ref :x ,new
                                                 :mark ,new-mark))
              ref))))

