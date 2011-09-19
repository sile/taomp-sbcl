(in-package :asdf)

(defsystem taomp
  :name "taomp"
  :author "Takeru Ohta"
  :description "A implementation of the algorithms described in 『The Art of Multiprocessor Programming』"
  :serial t
  :components ((:file "countdown-latch")
               (:file "tls")
               (:file "thread-id")
               (:file "atomic-stamped-reference")
               (:file "atomic-markable-reference")
               (:file "timeout")
               (:file "mutex-lock")
               (:file "util")
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

               ;; chapter 9
               (:file "c9/coarse-list")
               (:file "c9/fine-list")
               (:file "c9/optimistic-list")
               (:file "c9/lazy-list")
               (:file "c9/lock-free-list")

               ;; chapter 10
               (:file "c10/bounded-queue")
               (:file "c10/unbounded-lockfree-queue")

               ;; chapter 11
               (:file "c11/unbounded-lockfree-stack")
               (:file "c11/lockfree-exchanger")
               (:file "c11/elimination-array")
               (:file "c11/elimination-backoff-stack")

               ;; chapter 12
               (:file "c12/combining-tree")
               (:file "c12/bitonic")
               ))
