(defpackage util
  (:use :common-lisp)
  (:export while
           define-with-lock-macro
           clear
           
           tls-get
           tls-var
           make-tls

           atomic-uint))
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

(defstruct tls-var
  (var (make-hash-table :test #'eq :synchronized t) 
       :type hash-table))

(defun make-tls ()
  (make-tls-var))

(defun tls-get (tls-var)
  (gethash sb-thread:*current-thread* (tls-var-var tls-var)))

(defun (setf tls-get) (new-var tls-var)
  (setf (gethash sb-thread:*current-thread* (tls-var-var tls-var))
        new-var))

(deftype atomic-uint () #+X86-64 '(unsigned-byte 64)
                        #-X86-64 '(unsigned-byte 32))

