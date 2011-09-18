(defpackage coarse-list
  (:use :common-lisp :util)
  (:shadow :common-lisp remove)
  (:export make
           add
           remove
           contains?))
(in-package :coarse-list)

(defstruct node
  (key 0 :type fixnum)
  (item t :type t)
  (next nil :type (or null node)))

(defstruct @set
  (head t :type node)
  (lock (@make-lock) :type @lock))

(defparameter *tail* (make-node :key most-positive-fixnum))

(defun make ()
  (make-@set :head (make-node :key most-negative-fixnum :next *tail*)))

(defun size (set)
  (labels ((recur (n cnt)
             (if (eq n *tail*)
                 cnt
               (recur (node-next n) (1+ cnt)))))
    (recur (node-next (@set-head set)) 0)))

(defmethod print-object ((o @set) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "~a ~s ~s ~s ~a" :set :type :coarse :size (size o))))

(defun calc-key (item)
  (max (1+ most-negative-fixnum) (min (1- most-positive-fixnum) (sxhash item))))

(defun find-candidate (head key)
  (let ((prev head)
        (curr (node-next head)))
    (while (< (node-key curr) key)
      (setf prev curr
            curr (node-next curr)))
    (values prev curr)))

(defun add (set item &aux (key (calc-key item)))
  (with-slots (head lock) (the @set set)
    (@with-lock lock
      (multiple-value-bind (prev curr)
                           (find-candidate head key)
        (unless (= key (node-key curr))
          (let ((node (make-node :key key :item item)))
            (setf (node-next node) curr
                  (node-next prev) node)
            t))))))

(defun remove (set item &aux (key (calc-key item)))
  (with-slots (head lock) (the @set set)
    (@with-lock lock
      (multiple-value-bind (prev curr)
                           (find-candidate head key)
        (when (= key (node-key curr))
          (setf (node-next prev) (node-next curr))
          t)))))

(defun contains? (set item &aux (key (calc-key item)))
  (with-slots (head lock) (the @set set)
    (@with-lock lock
      (multiple-value-bind (prev curr)
                           (find-candidate head key)
        (declare (ignore prev))
        (= key (node-key curr))))))
