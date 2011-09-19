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

