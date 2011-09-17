(defpackage util
  (:use :common-lisp)
  (:export while
           define-with-lock-macro
           clear
           
           tls-get
           tls-var
           make-tls

           atomic-uint
           
           lock-test))
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

(defun lock-test (package-name &optional (thread-num 5) (loop-count 10))
  (util:clear)
  (let* ((package (find-package package-name))
         (lock-fn (find-symbol "LOCK" package))
         (unlock-fn (find-symbol "UNLOCK" package))
         (make-fn (find-symbol "MAKE" package)))
    
    (flet ((test-fn (lock begin-latch end-latch)
             (countdown-latch:countdown-and-await begin-latch)
             (loop WITH tid = (thread-id:get)
                   FOR i FROM 0 TO loop-count
               DO
               (funcall lock-fn lock)
               (when (zerop (mod i (max 1 (ceiling loop-count 5))))
                 (format t "~&; <THREAD ~a> ~a~%" tid i))
               (funcall unlock-fn lock))
             (countdown-latch:countdown-and-await end-latch)))
      (let ((begin-latch (countdown-latch:make thread-num))
            (end-latch (countdown-latch:make thread-num))
            (lock (funcall make-fn)))
        (loop REPEAT (1- thread-num)
              DO (sb-thread:make-thread
                  (lambda ()
                    (test-fn lock begin-latch end-latch))))
        (time
         (test-fn lock begin-latch end-latch)))))
  :done)
