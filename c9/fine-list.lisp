(defpackage fine-list
  (:use :common-lisp :util)
  (:shadow :common-lisp remove)
  (:export make
           add
           remove))
(in-package :fine-list)

(defstruct node
  (lock (@make-lock) :type @lock)
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
    (format stream "~a ~s ~s ~s ~a" :set :granularity :fine :size (size o))))

(defun calc-key (item)
  (max (1+ most-negative-fixnum) (min (1- most-positive-fixnum) (sxhash item))))

(defun find-candidate (head key)
  (let ((prev head)
        (curr (node-next head)))
    (@lock (node-lock curr))
    (unwind-protect
        (progn 
          (while (< (node-key curr) key)
            (@unlock (node-lock prev))
            (setf prev curr
                  curr (node-next curr))
            (@lock (node-lock curr)))
          (values prev curr))
      (@unlock (node-lock curr)))))

(defun add (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (@lock (node-lock head))
    (multiple-value-bind (prev curr)
                         (find-candidate head key)
      (prog1 (unless (= key (node-key curr))
               (let ((node (make-node :key key :item item)))
                 (setf (node-next node) curr
                       (node-next prev) node))
               t)
        (@unlock (node-lock prev))))))

(defun remove (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (@lock (node-lock head))
    (multiple-value-bind (prev curr)
                         (find-candidate head key)
      (prog1 (when (= key (node-key curr))
               (setf (node-next prev) (node-next curr))
               t)
        (@unlock (node-lock prev))))))

