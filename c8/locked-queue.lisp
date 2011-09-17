(defpackage locked-queue
  (:use :common-lisp)
  (:export make
           enq
           deq

           queue))
(in-package :locked-queue)

