(defpackage util
  (:use :common-lisp)
  (:export while))
(in-package :util)

(defmacro while (exp &body body)
  `(loop while ,exp do (locally ,@body)))
