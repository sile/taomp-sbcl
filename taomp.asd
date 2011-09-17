(in-package :asdf)

(defsystem taomp
  :name "taomp"
  :author "Takeru Ohta"
  :description "A implementation of the algorithms described in 『The Art of Multiprocessor Programming』"
  :serial t
  :components ((:file "countdown-latch")
               (:file "tls")
               (:file "thread-id")
               (:file "util")
               (:file "atomic-stamped-reference")
               (:file "timeout")
               (:file "mutex-lock")
               (:file "condition")

               ;; chapter 2
               (:file "c2/lock-one")
               (:file "c2/lock-two")
               (:file "c2/peterson-lock")
               (:file "c2/filter-lock")
               (:file "c2/bakery-lock")

               ;; chapter 7
               (:file "c7/tsa-lock")
               (:file "c7/ttsa-lock")
               (:file "c7/backoff")
               (:file "c7/backoff-lock")
               (:file "c7/a-lock")
               (:file "c7/clh-lock")
               (:file "c7/mcs-lock")
               (:file "c7/to-lock")
               (:file "c7/composit-lock")

               ;; chapter 8
               (:file "c8/locked-queue")
               (:file "c8/simple-readwrite-lock")
               (:file "c8/fifo-readwrite-lock")
               (:file "c8/simple-reentrant-lock")
               (:file "c8/semaphore")
               ))
