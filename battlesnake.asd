(asdf:defsystem #:battlesnake
  :depends-on (#:hunchentoot
	       #:drakma
	       #:cl-json
	       #:arrow-macros)
  :serial t
  :components ((:file "min-http")
	       (:file "battlesnake")))
