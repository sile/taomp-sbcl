(defpackage lazy-list
  (:use :common-lisp :util)
  (:shadow :common-lisp remove)
  (:export make
           add
           remove
           contains?))
(in-package :lazy-list)

(defstruct node
  (lock (@make-lock) :type @lock)
  (marked nil :type boolean)
  (key 0 :type fixnum)
  (item t :type t)
  (next nil :type (or null node)))

(defstruct @set
  (head t :type node))

(defun make ()
  (make-@set :head (make-node :key most-negative-fixnum 
                              :next (make-node :key most-positive-fixnum))))

(defun size (set)
  (labels ((recur (n cnt)
             (if (= (node-key n) most-positive-fixnum)
                 cnt
               (recur (node-next n) (1+ cnt)))))
    (recur (node-next (@set-head set)) 0)))

(defmethod print-object ((o @set) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "~a ~s ~s ~a" :set :lazy :size (size o))))

(defun calc-key (item)
  (max (1+ most-negative-fixnum) (min (1- most-positive-fixnum) (sxhash item))))

(defun find-candidate (head key)
  (let ((prev head)
        (curr (node-next head)))
    (while (< (node-key curr) key)
      (setf prev curr
            curr (node-next curr)))
    (values prev curr)))

(defun validate (pred curr)
  (and (not (node-marked pred))
       (not (node-marked curr))
       (eq (node-next pred) curr)))

(defun add (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (multiple-value-bind (prev curr)
                         (find-candidate head key)
      (@with-lock (node-lock prev)
        (@with-lock (node-lock curr)
          (when (validate prev curr)
            (unless (= key (node-key curr))
              (setf (node-next prev)
                    (make-node :key key :item item :next curr))
              t)))))))

(defun remove (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (multiple-value-bind (prev curr)
                         (find-candidate head key)
      (@with-lock (node-lock prev)
        (@with-lock (node-lock curr)
          (when (validate prev curr)
            (when (= key (node-key curr))
              (setf (node-next prev) (node-next curr)
                    (node-marked curr) t)
              t)))))))

(defun contains (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (multiple-value-bind (prev curr)
                         (find-candidate head key)
      (declare (ignore prev))
      (and (eq (node-key curr) key)
           (not (node-marked curr))))))
