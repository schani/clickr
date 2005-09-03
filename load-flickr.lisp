(require 'asdf)

(asdf:operate 'asdf:load-op 's-xml)
(asdf:operate 'asdf:load-op 's-xml-rpc)

(load "utils.lisp")

(load "md5.lisp")
(load "flickr.lisp")
(load "clickr.lisp")

(in-package :flickr)

(defparameter *schani* (user-with-name "schani"))
(defparameter *testerich* (user-with-name "test_er_ich"))
