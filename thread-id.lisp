(defpackage thread-id
  (:use :common-lisp)
  (:shadow :common-lisp get)
  (:export get))
(in-package :thread-id)

(tls:define *id* (calc-id))

(defun get () *id*)

(defun calc-id ()
  (position sb-thread:*current-thread*
            (reverse (sb-thread:list-all-threads))
            :test #'eq))
