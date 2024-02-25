(asdf:defsystem #:project-euler
  :depends-on (#:cl-coroutine
	       #:halp
	       #:halp/crypto)
  :serial t
  :components ((:file "solutions")))
