(require 'asdf)

(asdf:operate 'asdf:load-op 'md5)
(asdf:operate 'asdf:load-op 's-xml)
(asdf:operate 'asdf:load-op 's-xml-rpc)
(asdf:operate 'asdf:load-op 'trivial-sockets)
(asdf:operate 'asdf:load-op 'trivial-http)
(asdf:operate 'asdf:load-op 'cl-ppcre)

(load "utils.lisp")
(load "let-match.lisp")

(load "flickr.lisp")
(load "clickr.lisp")
;(load "clickr-utils.lisp")
(load "automatr.lisp")

(use-package '(:automatr :flickr :clickr))

(defun login-and-make-actions (user)
  (request-authorization)
  (sleep 10)
  (complete-authorization)
  (let ((actions (audit-actions (all-applicable-actions user))))
    (htmlize-actions actions)
    actions))

(defun reset-all ()
  (reset-clickr)
  (setq *me* (user-with-name *default-user-name*)))
