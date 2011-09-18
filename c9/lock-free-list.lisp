(defpackage lock-free-list
  (:use :common-lisp :util)
  (:shadow :common-lisp remove)
  (:export make
           add
           remove
           contains?))
(in-package :lock-free-list)

(defstruct node
  (key 0 :type fixnum)
  (item t :type t)
  (next nil :type (or null amr:object)))

(defstruct @set
  (head t :type node))

(defun make ()
  (make-@set 
   :head (make-node
          :key most-negative-fixnum 
          :next (amr:make 
                 (make-node :key most-positive-fixnum
                            :next (amr:make (make-node :key most-positive-fixnum)
                                            nil))
                 nil))))

(defun find-candidate (head key) 
  (labels ((recur1 (pred &aux (curr (amr:get (node-next pred))))
             (recur2 pred curr))

           (recur2 (pred curr)
             (multiple-value-bind (succ marked) (amr:get (node-next curr))
               (if marked  ; If MARKED is true, the node has been logically removed.
                   (if (not (amr:compare-and-set (node-next pred)
                                                 curr succ nil nil))
                       (recur1 head)
                     ;; The attempt to remove CURR node physically is succeeded
                     (recur2 pred succ))
                 (if (>= (node-key curr) key)
                     (values pred curr)
                   (recur2 curr succ))))))
    (recur1 head)))

(defun calc-key (item)
  (max (1+ most-negative-fixnum) (min (1- most-positive-fixnum) (sxhash item))))

(defun add (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (multiple-value-bind (pred curr) (find-candidate head key)
      (unless (= (node-key curr) key)
        (let ((node (make-node :key key :item item
                               :next (amr:make curr nil))))
          (if (amr:compare-and-set (node-next pred)
                                   curr node nil nil)
              t
            (add set item)))))))

(defun remove (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (multiple-value-bind (pred curr) (find-candidate head key)
      (when (= (node-key curr) key)
        (let ((succ (amr:get (node-next curr))))
          (if (not (amr:compare-and-set (node-next curr)
                                        succ succ nil t))
              (remove set item)
            (progn (amr:compare-and-set (node-next pred)
                                        curr succ nil nil)
                   t)))))))

(defun contains? (set item &aux (key (calc-key item)))
  (with-slots (head) (the @set set)
    (let ((curr head))
      (while (< (node-key curr) key)
        (setf curr (amr:get (node-next curr))))
      (and (= (node-key curr) key)
           (not (nth-value 1 (amr:get (node-next curr)))#|marked|#)))))
