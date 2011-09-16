(in-package :asdf)

(defsystem taomp
  :name "taomp"
  :author "Takeru Ohta"
  :description "A implementation of the algorithms described in 『The Art of Multiprocessor Programming』"
  :serial t
  :components ((:file "util")
               (:file "countdown-latch")
               (:file "tls")
               (:file "thread-id")

               ;; chapter 2
               (:file "c2/lock-one")
               (:file "c2/lock-two")
               (:file "c2/peterson-lock")
               (:file "c2/filter-lock")
               (:file "c2/bakery-lock")

               ;; chapter 7
               (:file "c7/tsa-lock")
               
               ))
