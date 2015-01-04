;;;; cl-distributed.asd

(asdf:defsystem #:cl-distributed
  :serial t
  :description "Describe cl-distributed here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-history #:house #:parenscript #:cl-who)
  :components ((:file "package")
               (:file "cl-distributed")))

