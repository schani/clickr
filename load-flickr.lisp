(use-package '(:automatr :flickr :clickr))

(defvar *api-key* "fe62be41f5e31734214d3efd4312dbb6")
(defvar *shared-secret* "357a7ea29d65120d")

(defvar *my-api-info* nil)
(defvar *my-user-name* "schani")
(defvar *my-user* nil)

(defun login ()
  (let ((api-info (request-authorization *api-key* *shared-secret*)))
    (sleep 10)
    (complete-authorization api-info)
    (setf *my-api-info* api-info)
    (setf *my-user* (user-with-name api-info *my-user-name*))))

(defun login-and-make-actions ()
  (login)
    (let* ((actions (audit-actions (all-applicable-actions *me*))))
      (htmlize-actions actions)
      actions))
