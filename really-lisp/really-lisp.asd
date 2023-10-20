(asdf:defsystem #:really-lisp
  :depends-on (#:hunchentoot
	       #:cl-who)
  :serial t
  :components ((:file "server")))
