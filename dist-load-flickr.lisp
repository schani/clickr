(use-package '(:automatr :flickr :clickr))

(defvar *api-key* "YOUR API KEY")
(defvar *shared-secret* "YOUR SHARED SECRET")

(defvar *my-user-name* "YOUR USER NAME")
(defvar *my-user* nil)

(defun login-and-make-actions ()
  (let ((api-info (request-authorization *api-key* *shared-secret*)))
    (sleep 10)
    (complete-authorization api-info)
    (let* ((*me* (user-with-name api-info *my-user-name*))
	   (actions (audit-actions (all-applicable-actions *me*))))
      (htmlize-actions actions)
      (setf *my-user* *me*)
      actions)))
