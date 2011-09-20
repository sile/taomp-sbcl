(defpackage base-hash-set
  (:use :common-lisp :util)
  (:nicknames base)
  (:export make
           add
           erase
           contains?

           @hash
           with-lock
           table
           set-size))
(in-package :base-hash-set)

(defstruct @hash
  (table #() :type vector)
  (set-size 0 :type fixnum)
  (acquire t :type function)
  (release t :type function)
  (policy t :type function)
  (resize t :type function))

(defun make (capacity &key (acquire (lambda ()))
                           (release (lambda ()))
                           (policy (lambda ()))
                           (resize (lambda ())))
  (make-@hash :table (make-array capacity :initial-element '())
              :acquire acquire
              :release release
              :policy policy
              :resize resize))

(defmacro with-lock ((hash x) &body body)
  (let ((acquire (gensym))
        (release (gensym))
        (hash~ (gensym))
        (x~ (gensym)))
    `(let ((,x~ ,x)
           (,hash~ ,hash))
       (with-slots ((,acquire acquire) (,release release)) (the @hash ,hash~)
         (funcall ,acquire ,hash~ ,x~)
         (unwind-protect
             (locally ,@body)
           (funcall ,release ,hash~ ,x~))))))

(defun get-bucket (table x)
  (aref table (mod (sxhash x) (length table))))

(defun set-bucket (table x new)
  (setf (aref table (mod (sxhash x) (length table)))
        new))

(defun contains? (hash x) 
  (with-slots (table) (the @hash hash)
    (with-lock (hash x)
      (let ((bucket (get-bucket table x)))
        (not (null (member x bucket)))))))

(defun add (hash x)
  (with-slots (table set-size policy resize) (the @hash hash)
    (prog1 (with-lock (hash x)
             (let* ((bucket (get-bucket table x))
                    (orig bucket))
               (pushnew x bucket)
               (when (not (eq bucket orig))
                 (set-bucket table x bucket)
                 (incf set-size)
                 t)))
      (when (funcall policy hash)
        (funcall resize hash)))))
