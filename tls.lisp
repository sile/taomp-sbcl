;; see: http://paste.lisp.org/display/63257
(defpackage tls
  (:use :common-lisp :sb-vm :sb-sys :sb-kernel)
  (:shadow :common-lisp symbol-value)
  (:export global-binding-p
           symbol-value
           define))
(in-package :sb-vm)

;; see cell.lisp:symbol-value
(eval-when (:compile-toplevel)
  (define-vop (tls::tls-ref)
    (:args (index :scs (descriptor-reg)))
    (:results (value :scs (descriptor-reg)))
    #+x86-64
    (:generator 5
                (inst mov value (make-ea :qword
                                         :base thread-base-tn
                                         :index index :scale 1)))
    #+x86
    (:generator 5
                (inst fs-segment-prefix)
                (inst mov value (make-ea :dword :base index))))
  
  (define-vop (tls::tls-set)
    (:args (value :scs (descriptor-reg))
           (index :scs (descriptor-reg)))
    (:results)
    #+x86-64
    (:generator 5
                (inst mov (make-ea :qword
                                   :base thread-base-tn
                                   :index index :scale 1)
                      value))
    #+x86
    (:generator 5
                (inst fs-segment-prefix)
                (inst mov (make-ea :dword :base index) value)))
  
  (define-vop (tls::%set-symbol-global-value)
    (:args (value  :scs (descriptor-reg))
           (symbol :scs (descriptor-reg)))
    (:results)
    #+(or x86-64 x86)
    (:generator 5
                (storew value symbol symbol-value-slot other-pointer-lowtag))))

(in-package :tls)

(defun global-binding-p (symbol)
  "Simply check that the symbol has no tls index,
   or that the tls slot is empty."
  (declare (type symbol symbol))
  (let ((index (sb-vm::symbol-tls-index symbol)))
    (or (zerop index)
        (eq (%primitive tls-ref index)
            (%make-lisp-obj no-tls-value-marker-widetag)))))

(defun ensure-tls-index (symbol)
  (declare (type symbol symbol))
  (let ((index (sb-vm::symbol-tls-index symbol)))
    (unless (zerop index)
      (return-from ensure-tls-index index)))
  ;; HACK make sure an index gets allocated.
  (progv (list symbol) (list nil)
    (sb-vm::symbol-tls-index symbol)))

(defun symbol-value (symbol)
  (declare (type symbol symbol))
  (let ((value (%primitive tls-ref (ensure-tls-index symbol))))
    (if (eq value (%make-lisp-obj no-tls-value-marker-widetag))
        (values nil nil)
        (values value t))))

(defun (setf symbol-value) (value symbol)
  (prog1 value
    (%primitive tls-set value (ensure-tls-index symbol))))

(defmacro define (name value)
  `(define-symbol-macro ,name 
     (values (or (tls:symbol-value ',name)
                 (setf (tls:symbol-value ',name) ,value)))))
