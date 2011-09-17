(defpackage util
  (:use :common-lisp)
  (:export while
           define-with-lock-macro
           define-with-trylock-macro
           clear
           
           tls-get
           tls-var
           make-tls

           atomic-uint
           
           get-and-set

           trylock-test
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

(defmacro define-with-trylock-macro (name lock unlock)
  `(defmacro ,name ((lock-obj timeout) &body body)
     (let ((obj (gensym)))
       `(let ((,obj ,lock-obj))
          (,',lock ,obj ,timeout)
          (unwind-protect
              (locally ,@body)
            (,',unlock ,obj))))))

(defun clear ()
  (mapc #'sb-thread:destroy-thread (butlast (sb-thread:list-all-threads))))

(defstruct tls-var
  (var (make-hash-table :test #'eq :synchronized t) 
       :type hash-table)
  (init (lambda ()) :type function))

(defun make-tls (&optional (init-fn (lambda())))
  (make-tls-var :init init-fn))

(defun tls-get (tls-var)
  (multiple-value-bind (x exists?)
                       (gethash sb-thread:*current-thread* (tls-var-var tls-var))
    (if exists?
        x
      (setf (tls-get tls-var) (funcall (tls-var-init tls-var))))))
      

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
                   FOR i FROM 0 BELOW loop-count
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

(defun trylock-test (package-name timeout &optional (thread-num 5) (loop-count 10))
  (util:clear)
  (let* ((package (find-package package-name))
         (lock-fn (find-symbol "TRYLOCK" package))
         (unlock-fn (find-symbol "UNLOCK" package))
         (make-fn (find-symbol "MAKE" package)))
    
    (flet ((test-fn (lock begin-latch end-latch)
             (countdown-latch:countdown-and-await begin-latch)
             (loop WITH tid = (thread-id:get)
                   WITH expired-count = 0
                   FOR i FROM 0 BELOW loop-count
               DO
               (if (not (funcall lock-fn lock timeout))
                   (incf expired-count)
                 (progn
                   (when (zerop (mod i (max 1 (ceiling loop-count 5))))
                     (format t "~&; <THREAD ~a> ~a~%" tid i))
                   (funcall unlock-fn lock)))
               FINALLY
               (funcall lock-fn lock 1000)
               (format t "~&; <THREAD ~a> expired count: ~a~%" tid expired-count)
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

(defmacro get-and-set (place new)
  `(loop FOR old = ,place THEN old~
         FOR old~ = (sb-ext:compare-and-swap ,place old ,new)
         UNTIL (eq old old~)
         FINALLY
         (return old)))
