(asdf:defsystem #:foolander
  :depends-on (#:calispel
	       #:cl-who
	       #:hunchentoot)
  :serial t
  :components ((:file "server")))
