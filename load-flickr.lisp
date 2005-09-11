(require 'asdf)

(asdf:operate 'asdf:load-op 's-xml)
(asdf:operate 'asdf:load-op 's-xml-rpc)
(asdf:operate 'asdf:load-op 'trivial-sockets)
(asdf:operate 'asdf:load-op 'trivial-http)
(asdf:operate 'asdf:load-op 'cl-ppcre)

(load "utils.lisp")
(load "let-match.lisp")

(load "md5.lisp")
(load "flickr.lisp")
(load "clickr.lisp")

(in-package :flickr)

(defparameter *schani* (user-with-name "schani"))
(defparameter *testerich* (user-with-name "test_er_ich"))

(load "automatr.lisp")
