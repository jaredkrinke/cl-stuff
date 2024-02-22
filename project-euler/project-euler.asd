(asdf:defsystem #:project-euler
  :depends-on (#:halp
	       #:halp/crypto)
  :serial t
  :components ((:file "solutions")))
