(asdf:defsystem #:really-lisp
  :depends-on (#:calispel
	       #:cl-who
	       #:hunchentoot)
  :serial t
  :components ((:file "server")))
