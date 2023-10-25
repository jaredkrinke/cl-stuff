(require "asdf")
(pushnew :hunchentoot-no-ssl *features*)
(asdf:load-system :really-lisp)
(really-lisp/server:start-server)
