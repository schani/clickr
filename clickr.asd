;;;; -*- indent-tabs-mode: nil; mode: lisp -*-

(defsystem clickr
	:name "clickr"
        :author "Mark Probst"
        :licence "GPL"
        :description "High-level Flickr interface"
        :depends-on (:trivial-http :md5 :s-xml :s-xml-rpc
                                   :cl-ppcre)
        :serial t
        :components ((:file "utils")
                     (:file "let-match")
                     (:file "flickr")
                     (:file "clickr")
                     (:file "automatr")
                     ))
