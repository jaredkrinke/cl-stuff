(asdf:defsystem #:battlesnake
  :depends-on (#:hunchentoot
	       #:cl-json
	       #:arrow-macros)
  :serial t
  :components ((:file "min-http")
	       (:file "battlesnake")))
