;;;; cl-distributed.asd

(asdf:defsystem #:cl-distributed
  :serial t
  :description "Describe cl-distributed here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-history)
  :components ((:file "package")
               (:file "cl-distributed")))

