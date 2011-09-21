(defpackage lockfree-hashset
  (:use :common-lisp :util)
  (:export make
           add
           erase
           contains?))
(in-package :lockfree-hashset)

(defconstant +MAX_BIT+ (1- (integer-length most-positive-fixnum)))

(defstruct node
  (key t :type fixnum)
  (value t :type t)
  (next nil :type (or null amr:object)))

(defstruct bucket-list 
  (head t :type node))

(defun bit-reverse (int size)
  (loop FOR i FROM 0 BELOW size
        FOR j FROM (1- size) DOWNTO 0
        WHILE (< i j)
    DO
    (rotatef (ldb (byte 1 i) int)
             (ldb (byte 1 j) int)))
  int)

(defun ordinary-key (x)
  (bit-reverse (dpb 1 (byte 1 +MAX_BIT+)
                    (ldb (byte +MAX_BIT+ 0) (sxhash x)))
               (1+ +MAX_BIT+)))

(defun sentinel-key (key)
  (bit-reverse (dpb 0 (byte 1 +MAX_BIT+) key) (1+ +MAX_BIT+)))

(defun list-make ()
  (make-bucket-list
   :head (make-node
          :key 0
          :next (amr:make 
                 (make-node :key most-positive-fixnum
                            :next (amr:make (make-node :key most-positive-fixnum)
                                            nil))
                 nil))))

(defun list-find-candidate (head key) 
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

(defun list-contains? (list x &aux (key (ordinary-key x)))
  (with-slots (head) (the bucket-list list)
    (multiple-value-bind (prev curr) (list-find-candidate head key)
      (declare (ignore prev))
      (= (node-key curr) key))))

(defun list-get-sentinel (list index &aux (key (sentinel-key index)))
  (with-slots (head) (the bucket-list list)
    (loop
     (multiple-value-bind (pred curr) (list-find-candidate head key)
       (when (= (node-key curr) key)
         (return (make-bucket-list :head curr)))

       (let* ((node (make-node :key key
                               :next (amr:make (amr:get (node-next pred))
                                               nil)))
              (splice (amr:compare-and-set (node-next pred)
                                           curr node nil nil)))
         (when splice
           (return (make-bucket-list :head node))))))))

(defun list-add (set item &aux (key (ordinary-key item)))
  (with-slots (head) (the bucket-list set)
    (multiple-value-bind (pred curr) (list-find-candidate head key)
      (unless (= (node-key curr) key)
        (let ((node (make-node :key key :value item
                               :next (amr:make curr nil))))
          (if (amr:compare-and-set (node-next pred)
                                   curr node nil nil)
              t
            (add set item)))))))


(defstruct lockfree-hashset 
  (bucket t :type (vector (or null bucket-list)))
  (bucket-size 0 :type fixnum)
  (set-size 0 :type atomic-uint))

(defun make (capacity)
  (let ((ary (make-array capacity :element-type '(or null bucket-list)
                         :initial-element nil)))
    (setf (aref ary 0) (list-make))
    (make-lockfree-hashset :bucket ary
                           :bucket-size 2
                           :set-size 0)))

(defconstant +THRESHOLD+ 4)

(defun get-parent (bucket-size my-bucket)
  (let ((parent bucket-size))
    (loop
     (setf parent (ash parent -1))
     (when (<= parent my-bucket)
       (return)))
    (- my-bucket parent)))

(defun initialize-bucket (bucket bucket-size my-bucket)
  (let ((parent (get-parent bucket-size my-bucket)))
    (unless (aref bucket parent)
      (initialize-bucket bucket bucket-size parent))
    (setf (aref bucket my-bucket)
          (list-get-sentinel (aref bucket parent) my-bucket))))

(defun get-bucket-list (bucket bucket-size my-bucket)
  (unless (aref bucket my-bucket)
    (initialize-bucket bucket bucket-size my-bucket))
  (aref bucket my-bucket))

(defun add (set x)
  (with-slots (bucket bucket-size set-size) (the lockfree-hashset set)
    (let* ((my-bucket (mod (sxhash x) bucket-size))
           (b (get-bucket-list bucket bucket-size my-bucket)))
      (when (list-add b x)
        (let ((set-size-now (sb-ext:atomic-incf (lockfree-hashset-set-size set)))
              (bucket-size-now bucket-size))
          (when (> (/ set-size-now bucket-size-now) +THRESHOLD+)
            (sb-ext:compare-and-swap (lockfree-hashset-bucket-size set)
                                     bucket-size-now (* 2 bucket-size-now))))
        t))))
