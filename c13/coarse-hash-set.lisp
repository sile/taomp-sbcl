(defpackage coarse-hash-set
  (:use :common-lisp :util)
  (:export make
           add
           erase
           contains?))
(in-package :coarse-hash-set)

(defstruct @hash
  (super t :type base-hash-set::@hash))

(defun gen-acquire (lock)
  (lambda (_1 _2)
    (declare (ignore _1 _2))
    (@lock lock)))

(defun gen-release (lock)
  (lambda (_1 _2)
    (declare (ignore _1 _2))
    (@unlock lock)))

(defun gen-policy ()
  (lambda (base-hash)
    (> (floor (base-hash-set::@hash-set-size base-hash)
              (length (base-hash-set::@hash-table base-hash)))
       4)))

(defun gen-resize (lock)
  (lambda (base-hash)
    (with-slots ((table base-hash-set::table) 
                 (set-size base-hash-set::set-size)) base-hash
     (let ((old-capacity (length table)))
       (base-hash-set::with-lock (base-hash lock)
         (if (/= old-capacity (length table))
             nil ; someone beat us to it
           (let ((new-capacity (* 2 old-capacity))
                 (old-table table))
             (setf table (make-array new-capacity :initial-element '()))
             (loop FOR bucket ACROSS old-table
               DO
               (dolist (x bucket)
                 (push x (aref table (mod (sxhash x) (length table)))))))))))))
             

(defun make (capacity)
  (let ((lock (@make-lock)))
    (make-@hash 
     :super (base-hash-set:make 
             capacity
             :acquire (gen-acquire lock)
             :release (gen-release lock)
             :resize (gen-resize lock)
             :policy (gen-policy)))))

(defun add (hash x)
  (base-hash-set:add (@hash-super hash) x))

(defun contains? (hash x)
  (base-hash-set:contains? (@hash-super hash) x))
