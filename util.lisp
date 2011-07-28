(defpackage util
  (:use :common-lisp)
  (:export while
           define-with-lock-macro
           clear))
(in-package :util)

(defmacro while (exp &body body)
  `(loop while ,exp do (locally ,@body)))

(defmacro define-with-lock-macro (name lock unlock)
  `(defmacro ,name (lock-obj &body body)
     (let ((obj (gensym)))
       `(let ((,obj ,lock-obj))
          (,',lock ,obj)
          (unwind-protect
              (locally ,@body)
            (,',unlock ,obj))))))

(defun clear ()
  (mapc #'sb-thread:destroy-thread (butlast (sb-thread:list-all-threads))))
