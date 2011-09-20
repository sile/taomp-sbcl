(defpackage striped-hash-set
  (:use :common-lisp :util)
  (:export make
           add
           erase
           contains?))
(in-package :striped-hash-set)

(defstruct @hash
  (super t :type base:@hash))

(defun gen-acquire (locks)
  (lambda (_ x)
    (declare (ignore _))
    (@lock (aref locks (mod (sxhash x) (length locks))))))

(defun gen-release (locks)
  (lambda (_ x)
    (declare (ignore _))
    (@unlock (aref locks (mod (sxhash x) (length locks))))))

(defun gen-policy ()
  (lambda (base-hash)
    (> (floor (base-hash-set::@hash-set-size base-hash)
              (length (base-hash-set::@hash-table base-hash)))
       4)))

(defun gen-resize (locks)
  (lambda (base-hash)
    (with-slots ((table base-hash-set::table) 
                 (set-size base-hash-set::set-size)) base-hash
     (let ((old-capacity (length table)))
       (loop FOR l ACROSS locks DO (@lock l))
       (unwind-protect
         (if (/= old-capacity (length table))
             nil ; someone beat us to it
           (let ((new-capacity (* 2 old-capacity))
                 (old-table table))
             (setf table (make-array new-capacity :initial-element '()))
             (loop FOR bucket ACROSS old-table
               DO
               (dolist (x bucket)
                 (push x (aref table (mod (sxhash x) (length table))))))))
         (loop FOR l ACROSS locks DO (@unlock l)))))))

(defun make (capacity)
  (let ((locks (coerce (loop REPEAT capacity 
                             COLLECT (@make-lock))
                       'vector)))
    (make-@hash 
     :super (base-hash-set:make 
             capacity
             :acquire (gen-acquire locks)
             :release (gen-release locks)
             :resize (gen-resize locks)
             :policy (gen-policy)))))

(defun add (hash x)
  (base-hash-set:add (@hash-super hash) x))

(defun contains? (hash x)
  (base-hash-set:contains? (@hash-super hash) x))
