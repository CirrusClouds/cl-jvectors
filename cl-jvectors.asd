;;;; cl-jvectors.asd

(asdf:defsystem #:cl-jvectors
  :description "Just another linear algebra package with Test-Driven Development"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-utter #:cl-utils #:vgplot)
  :components ((:file "package")
               (:file "cl-jvectors")
               (:file "fourier")))
