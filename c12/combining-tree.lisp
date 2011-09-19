(defpackage combining-tree
  (:use :common-lisp :util)
  (:export make
           get-and-increment))
(in-package :combining-tree)

(deftype node-status () '(member :idle :first :second :result :root))

(defstruct node
  (status :idle :type node-status)
  (locked nil :type boolean)
  (first-value 0 :type fixnum)
  (second-value 0 :type fixnum)
  (result 0 :type fixnum)
  (parent nil :type (or null node)))

(defmethod print-object ((o node) stream)
  (print-unreadable-object (o stream :identity t)
    (with-slots (status locked first-value second-value result parent) o
      (format stream "~a ~s ~s ~s ~s ~s ~s" 
              :node status (if locked :locked :unlocked)
              first-value second-value result parent))))

(defun root-node ()
  (make-node :status :root))

(defun node (parent)
  (make-node :parent parent))


(defstruct combining-tree
  (nodes #() :type (vector node))
  (leaf #() :type (vector node)))

(defun make (width)
  (let ((nodes (make-array (1- width) :element-type 'node
                                      :initial-element (root-node)))
        (leaf (make-array (floor (1+ width) 2) :element-type 'node
                                               :initial-element (root-node))))
    (loop FOR i FROM 1 BELOW (length nodes) 
          DO (setf (aref nodes i) (node (aref nodes (floor (1- i) 2)))))

    (loop FOR i FROM 0 BELOW (length leaf)
          DO (setf (aref leaf i) (aref nodes (- (length nodes) i 1))))

    (make-combining-tree :nodes nodes :leaf leaf)))

(defun wait ()
  (sleep 0.001))

(defun notify-all ()
  'noop)

(defun precombine (node)
  (with-slots (status locked) (the node node)
    (while locked (wait))

    (case status
      (:idle (setf status :first)
             t)
      (:first (setf locked t
                    status :second)
              nil)
      (:root  nil)
      (otherwise 
       (error "PanicException: unexpected Node state# ~a" status)))))

(defun combine (node combined)
  (with-slots (status locked first-value second-value) (the node node)
    (while locked (wait))
    
    (setf locked t
          first-value combined)

    (case status
      (:first first-value)
      (:second (+ first-value second-value))
      (otherwise 
       (error "PanicException: unexpected Node state# ~a" status)))))

(defun op (node combined) 
  (with-slots (locked status result second-value) (the node node)
    (case status
      (:root 
       (prog1 result
         (incf result combined)))
      (:second 
       (setf second-value combined
             locked nil)
       (notify-all)
       (while (not (eq status :result))
         (wait))
       (setf locked nil)
       (notify-all)
       (setf status :idle)
       result)
      (otherwise 
       (error "PanicException: unexpected Node state# ~a" status)))))

(defun distribute (node prior)
  (with-slots (locked status result first-value) (the node node)
    (case status
      (:first 
       (setf status :idle
             locked nil))
      (:second 
       (setf result (+ prior first-value)
             status :result))
      (otherwise 
       (error "PanicException: unexpected Node state# ~a" status)))))

(defun get-and-increment (combining-tree)
  (with-slots (nodes leaf) (the combining-tree combining-tree)
    (let* ((stack '())
          (my-leaf (aref leaf (floor (thread-id:get) 2)))
          (node my-leaf))
      
      ;; precombining phase
      (while (precombine node)
        (setf node (node-parent node)))

      (let ((stop node)
            (node my-leaf)
            (combined 1))
        ;; combining phase
        (while (not (eq node stop))
          (setf combined (combine node combined))
          (push node stack)
          (setf node (node-parent node)))

        ;; operation phase
        (let ((prior (op stop combined)))
          
          ;; distribution phase
          (dolist (node stack)
            (distribute node prior))

          prior)))))

#|
;; for test
(let* ((n 32)
       (latch (countdown-latch:make (1- n)))
       (ct (combining-tree:make n))
       (rlt '()))
  (dolist (th (loop REPEAT (1- n)
                COLLECT
                (sb-thread:make-thread
                 (lambda (&aux (id (thread-id:get)))
                   (countdown-latch:countdown-and-await latch)
                   (loop FOR i FROM 0 BELOW 32
                         FOR x = (progn (sb-thread:thread-yield)
                                        (combining-tree:get-and-increment ct))
                     COLLECT (list x id i))))))
    (setf rlt (append rlt (sb-thread:join-thread th))))
  (loop FOR (num thread-id loop-i) IN (sort rlt #'< :key #'car)
        DO
        (format t "~&; ~a# THREAD:~a LOOP:~a~%" num thread-id loop-i)))
|#