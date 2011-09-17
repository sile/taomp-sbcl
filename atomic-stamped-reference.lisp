(defpackage atomic-stamped-reference
  (:use :common-lisp)
  (:shadow :common-lisp get set)
  (:nicknames asr)
  (:export make
           get
           set
           compare-and-set))
(in-package :atomic-stamped-reference)

(defstruct ref
  (x t :type t)
  (stamp 0 :type fixnum))

(defstruct asr
  (ref nil :type ref))

(defmethod print-object ((o asr) stream)
  (print-unreadable-object (o stream)
    (format stream "~a ~s ~s ~s ~s" 
            :asr :value (ref-x (asr-ref o)) :stamp (ref-stamp (asr-ref o)))))
  
(defun make (val initial-stamp)
  (make-asr :ref (make-ref :x val :stamp initial-stamp)))

(defun get (obj)
  (let ((o (asr-ref obj)))
    (values (ref-x o) (ref-stamp o))))

(defun set (obj val stamp)
  (setf (asr-ref obj)
        (make-ref :x val :stamp stamp))
  obj)

(defmacro compare-and-set (obj expected new expected-stamp new-stamp)
  `(let ((ref (asr-ref ,obj)))
     (and (= (ref-stamp ref) ,expected-stamp)
          (eq (ref-x ref) ,expected)
          (eq (sb-ext:compare-and-swap (asr-ref ,obj)
                                       ref
                                       (make-ref :x ,new
                                                 :stamp ,new-stamp))
              ref))))
