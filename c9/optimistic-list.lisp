(defpackage optimistic-list
  (:use :common-lisp :util)
  (:shadow :common-lisp remove)
  (:export make
           add
           remove))
(in-package :optimistic-list)

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
    (format stream "~a ~s ~s ~a" :set :optimistic :size (size o))))

(defun calc-key (item)
  (max (1+ most-negative-fixnum) (min (1- most-positive-fixnum) (sxhash item))))

(defun find-candidate (head key)
  (let ((prev head)
        (curr (node-next head)))
    (while (< (node-key curr) key)
      (setf prev curr
            curr (node-next curr)))
    (values prev curr)))

(defun validate (head pred curr &aux (node head))
  (while (<= (node-key node) (node-key pred))
    (when (eq node pred)
      (return-from validate (eq (node-next pred) curr)))
    (setf node (node-next node)))
  nil)

(defun add (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (multiple-value-bind (prev curr)
                         (find-candidate head key)
      (@with-lock (node-lock prev)
        (@with-lock (node-lock curr)
          (when (validate head prev curr)
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
          (when (validate head prev curr)
            (when (= key (node-key curr))
              (setf (node-next prev) (node-next curr))
              t)))))))
