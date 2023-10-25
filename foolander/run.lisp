(require "asdf")
(pushnew :hunchentoot-no-ssl *features*)
(asdf:load-system :foolander)
(foolander:start-server)
