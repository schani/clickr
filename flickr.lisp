(defpackage "FLICKR"
  (:use "CL" "CCL" "S-XML" "S-XML-RPC" "MD5" "UTILS" "TRIVIAL-HTTP" "LET-MATCH")
  (:export

   ))

(in-package :flickr)

(defvar *api-key* "fe62be41f5e31734214d3efd4312dbb6")
(defvar *shared-secret* "357a7ea29d65120d")
(defvar *frob* nil)
(defvar *auth-token* nil)
(defvar *user* nil)

(defvar *debug-calls* nil)
(defvar *debug-call-results* nil)
(defvar *last-call-result* nil)

(defun kassoc (item list)
  (cond ((null list)
	 nil)
	((eql (car list) item)
	 (cadr list))
	(t
	 (kassoc item (cddr list)))))

(defun octets-to-md5-string (sequence)
  (string-downcase (apply #'concatenate 'string
			  (map 'list (lambda (n)
				       (concatenate 'string
						    (string (digit-char (ash n -4) 16))
						    (string (digit-char (logand n 15) 16))))
			       sequence))))

(defun md5sum-string (string)
  (octets-to-md5-string (md5sum-sequence string)))

(defun xml-tag (xml)
  (if (listp (car xml))
      (caar xml)
      (car xml)))

(defun xml-attrib (name xml)
  (kassoc name (cdar xml)))

(defun xml-child (name xml)
  (find-if #'(lambda (x)
	       (and (listp x) (eql (xml-tag x) name)))
	   (cdr xml)))

(defun xml-children (xml)
  (remove-if #'(lambda (x) (not (listp x)))
	     (cdr xml)))

(defun xml-body (xml)
  (car (last xml)))

(defun xml-follow-path (xml path)
  (cond ((null path)
	 xml)
	((eql path :body)
	 (xml-body xml))
	((symbolp path)
	 (xml-attrib path xml))
	(t
	 (xml-follow-path (xml-child (car path) xml)
			  (cdr path)))))

(defun convert-type (value type)
  (if (stringp value)
      (case type
	(:string value)
	(:integer (parse-integer value))
	(:boolean (not (zerop (parse-integer value))))
	(t (error "Unknown type ~A" type)))
      :unknown))

(defmacro defapistruct (name &body members)
  (let ((constructor-name (gensym))
	(parser-name (intern (format nil "MAKE-~A" name))))
    `(progn
      (defstruct (,name (:constructor ,constructor-name))
	,@(mapcar #'car members))
      (defun ,parser-name (xml)
	(,constructor-name
	 ,@(mappend #'(lambda (member)
			(destructuring-bind (name path &optional (type :string))
			    member
			  (let ((member-keyword (intern (symbol-name name) (find-package 'keyword))))
			    (if (and (listp path) (eql (car path) 'function))
				`(,member-keyword (funcall ,path xml))
				`(,member-keyword (convert-type (xml-follow-path xml ',path) ,type))))))
		    members))))))

(defapistruct flickr-user
  (id :|nsid|)
  (username (:|username| . :body)))

(defapistruct flickr-search-photo
  (id :|id|)
  (owner :|owner|)
  (secret :|secret|)
  (server :|server|)
  (title :|title|)
  (ispublic :|ispublic| :boolean)
  (isfriend :|isfriend| :boolean)
  (isfamily :|isfamily| :boolean))

(defapistruct flickr-photoset-photo
  (id :|id|)
  (secret :|secret|)
  (server :|server|)
  (title :|title|)
  (isprimary :|isprimary| :boolean))

(defapistruct flickr-full-photo
  (id :|id|)
  (secret :|secret|)
  (server :|server|)
  (isfavorite :|isfavorite| :boolean)
  (license :|license|)
  (rotation :|rotation|)
  (owner (:|owner| . :|nsid|))
  (title (:|title| . :body))
  (description (:|description| . :body))
  (ispublic (:|visibility| . :|ispublic|) :boolean)
  (isfriend (:|visibility| . :|isfriend|) :boolean)
  (isfamily (:|visibility| . :|isfamily|) :boolean)
  (posted (:|dates| . :|posted|))
  (taken (:|dates| . :|taken|))
  (takengranularity (:|dates| . :|takengranularity|))
  (lastupdate (:|dates| . :|lastupdate|))
  (permcomment (:|permissions| . :|permcomment|) :boolean)
  (permaddmeta (:|permissions| . :|permaddmeta|) :boolean)
  (cancomment (:|editability| . :|cancomment|) :boolean)
  (canaddmeta (:|editability| . :|canaddmeta|) :boolean)
  (comments (:|comments| . :body) :integer)
  (notes #'(lambda (xml)
	     (mapcar #'make-flickr-note (xml-children (xml-child :|notes| xml)))))
  (tags #'(lambda (xml)
	    (mapcar #'make-flickr-tag (xml-children (xml-child :|tags| xml)))))
  (urls #'(lambda (xml)
	    (mapcar #'make-flickr-url (xml-children (xml-child :|urls| xml))))))

(defapistruct flickr-note
  (id :|id|)
  (author :|author|)
  (authorname :|authorname|)
  (x :|x| :integer)
  (y :|y| :integer)
  (w :|w| :integer)
  (h :|h| :integer)
  (text :body))

(defapistruct flickr-tag
  (id :|id|)
  (author :|author|)
  (raw :|raw|)
  (text :body))

(defapistruct flickr-url
  (type :|type|)
  (url :body))

(defapistruct flickr-photoset
  (id :|id|)
  (primary :|primary|)
  (photos :|photos|)
  (secret :|secret|)
  (server :|server|)
  (title (:|title| . :body))
  (description (:|description| . :body)))

(defapistruct flickr-photoset-info
  (id :|id|)
  (owner :|owner|)
  (primary :|primary|)
  (photos :|photos|)
  (title (:|title| . :body))
  (description (:|description| . :body)))

(defapistruct flickr-person
  (id :|nsid|)
  (isadmin :|isadmin| :boolean)
  (ispro :|ispro| :boolean)
  (iconserver :|iconserver|)
  (username (:|username| . :body))
  (realname (:|realname| . :body))
  (mbox-sha1sum (:|mbox_sha1sum| . :body))
  (location (:|location| . :body))
  (firstdate (:|photos| :|firstdate| . :body))
  (firstdatetaken (:|photos| :|firstdatetaken| . :body))
  (count (:|photos| :|count| . :body) :integer))

(defapistruct flickr-group
  (id :|id|)
  (title (:|name| . :body))
  (description (:|description| . :body))
  (members (:|members| . :body) :integer)
  (privacy (:|privacy| . :body)))

(defapistruct flickr-list-group
  (id :|nsid|)
  (title :|name|)
  (admin :|admin|)
  (eighteenplus :|eighteenplus| :boolean))

(defapistruct flickr-context-set
  (id :|id|)
  (title :|title|))

(defapistruct flickr-context-pool
  (id :|id|)
  (title :|title|))

(defapistruct flickr-public-contact
  (id :|nsid|)
  (username :|username|)
  (ignored :|ignored| :boolean))

(defapistruct flickr-contact
  (id :|nsid|)
  (username :|username|)
  (realname :|realname|)
  (isfriend :|friend| :boolean)
  (isfamily :|family| :boolean)
  (ignored :|ignored| :boolean))

(defapistruct flickr-favorite
  (id :|id|)
  (owner :|owner|)
  (secret :|secret|)
  (server :|server|)
  (title :|title|)
  (ispublic :|ispublic| :boolean)
  (isfriend :|isfriend| :boolean)
  (isfamily :|isfamily| :boolean))

(defapistruct flickr-size
  (label :|label|)
  (width :|width| :integer)
  (height :|height| :integer)
  (source :|source|)
  (url :|url|))

(defun arguments-signature (args)
  (labels ((convert (args)
	     (if (null args)
		 nil
		 (cons (list (symbol-name (car args)) (cadr args)) (convert (cddr args))))))
    (let* ((sorted-args (sort (convert args) #'(lambda (x y) (string< (car x) (car y)))))
	   (args-string (apply #'concatenate 'string
			       (mapcar #'(lambda (a) (concatenate 'string (car a) (cadr a)))
				       sorted-args))))
      (md5sum-string (concatenate 'string *shared-secret* args-string)))))

(defun make-flickr-call (method string-modifier &rest args)
  (let* ((full-args (append (list :|api_key| *api-key*)
			    (if (not (null *auth-token*))
				(list :|auth_token| *auth-token*)
				'())
			    args))
	 (args-with-signature (append (list :|api_sig| (arguments-signature full-args))
				      full-args))
	 (encoded-call (encode-xml-rpc-call method (apply #'xml-rpc-struct args-with-signature))))
    (when *debug-calls*
      (format t "making XML-RPC call ~A with args ~A~%" method args-with-signature))
    (let* ((result-string (xml-rpc-call encoded-call
					:host "www.flickr.com"
					:url "http://www.flickr.com/services/xmlrpc/"))
	   (xml (parse-xml-string (funcall string-modifier result-string))))
      (when *debug-call-results*
	(format t "got result ~A~%" xml))
      (setq *last-call-result* xml)
      xml)))

(defun lispify-method-name (string)
  (apply #'concatenate 'string (map 'list #'(lambda (c)
					      (cond ((eql c #\.)
						     "-")
						    ((upper-case-p c)
						     (concatenate 'string "-" (string c)))
						    (t
						     (string (char-upcase c)))))
				    string)))

(defmacro defcall (name-string args &body body)
  (let ((full-method-name-string (concatenate 'string "flickr." name-string))
	(fun-name (intern (lispify-method-name name-string))))
    `(defun ,fun-name ,args
      (labels ((call (&rest args)
		 (apply #'make-flickr-call ,full-method-name-string #'identity args))
	       (call-with-string-modifier (string-modifier &rest args)
		 (apply #'make-flickr-call ,full-method-name-string string-modifier args)))
	,@body))))

;; returns a list of the photos, the total number of pages, and the
;; total number of photos.
(defun multi-page-call (call-fun make-fun per-page page &rest args)
  (let ((result (apply call-fun :|per_page| (format nil "~A" per-page) :|page| (format nil "~A" page) args)))
    (values (mapcar make-fun (xml-children result))
	    (parse-integer (xml-attrib :|pages| result))
	    (parse-integer (xml-attrib :|total| result)))))

(defcall "auth.getFrob" ()
  (xml-body (call)))

;; returns the token, the permission string, and the user
(defcall "auth.getToken" ()
  (let ((result (call :|frob| *frob*)))
    (values (xml-body (xml-child :|token| result))
	    (xml-body (xml-child :|perms| result))
	    (make-flickr-user (xml-child :|user| result)))))

(defcall "contacts.getList" (&key (filter nil))
  (let ((result (if (null filter)
		    (call)
		    (call :|filter| filter))))
    (mapcar #'make-flickr-contact (xml-children result))))

(defcall "contacts.getPublicList" (user-id)
  (let ((result (call :|user_id| user-id)))
    (mapcar #'make-flickr-public-contact (xml-children result))))

(defcall "favorites.getList" (user-id &key (per-page 50) (page 1))
  (multi-page-call #'call #'make-flickr-favorite per-page page :|user_id| user-id))

(defcall "favorites.getPublicList" (user-id &key (per-page 50) (page 1))
  (multi-page-call #'call #'make-flickr-favorite per-page page :|user_id| user-id))

(defcall "groups.getInfo" (group-id)
  (make-flickr-group (call :|group_id| group-id)))

(defcall "groups.pools.getPhotos" (group-id &key (per-page 50) (page 1) tags)
  (let ((optional-args (if (null tags)
			   '()
			   (list :|tags| tags))))
    (apply #'multi-page-call
	   #'call #'make-flickr-search-photo
	   per-page page
	   :|group_id| group-id optional-args)))

(defcall "people.findByUsername" (name)
  (make-flickr-user (call :|username| name)))

(defcall "people.getInfo" (user-id)
  (make-flickr-person (call :|user_id| user-id)))

(defcall "people.getPublicGroups" (user-id)
  (let ((result (call :|user_id| user-id)))
    (mapcar #'make-flickr-list-group (xml-children result))))

;(defcall "people.getPublicPhotos" (user-id &key (per-page 50) (page 1))
;  (let ((result (call :|user_id| user-id :|per_page| (format nil "~A" per-page) :|page| (format nil "~A" page))))
;    (values (mapcar #'make-flickr-photo (xml-children result))
;	    (parse-integer (xml-attrib :|pages| result))
;	    (parse-integer (xml-attrib :|total| result)))))

(defcall "photos.addTags" (photo-id tags)
  (let ((tags-string (format nil "~{\"~A\"~^ ~}" tags)))
    (call :|photo_id| photo-id :|tags| tags-string)))

(defcall "photos.getAllContexts" (photo-id)
  (let ((result (call-with-string-modifier #'(lambda (s)
					       (format nil "<list>~A</list>" s))
					   :|photo_id| photo-id)))
    (mapcar #'(lambda (item)
		(case (xml-tag item)
		  (:|set| (make-flickr-context-set item))
		  (:|pool| (make-flickr-context-pool item))
		  (t (error "invalid context tag ~A" (xml-tag item)))))
	    (xml-children result))))

(defcall "photos.getInfo" (photo-id &key secret)
  (let ((optional-args (if (not (null secret))
			   (list :|secret| secret)
			   '())))
    (make-flickr-full-photo (apply #'call :|photo_id| photo-id optional-args))))

(defcall "photos.getSizes" (photo-id)
  (mapcar #'make-flickr-size (xml-children (call :|photo_id| photo-id))))

(defcall "photos.search" (&key (per-page 50) (page 1)
			       user-id tags tag-mode text min-upload-date max-upload-date
			       min-taken-data max-taken-date license sort)
  (let ((optional-args (mappend #'(lambda (name value)
				    (if (not (null value))
					(list name value)
					'()))
				'(:|user_id| :|tags| :|tag_mode| :|text| :|min_upload_date| :|max_upload_date|
				  :|min_taken_date| :|max_taken_date| :|license| :|sort|)
				(list user-id tags tag-mode text min-upload-date max-upload-date
				      min-taken-data max-taken-date license sort))))
    (apply #'multi-page-call
	   #'call #'make-flickr-search-photo
	   per-page page
	   optional-args)))

(defcall "photosets.getInfo" (photoset-id)
  (let ((result (call :|photoset_id| photoset-id)))
    (make-flickr-photoset-info result)))

(defcall "photosets.getList" (user-id)
  (let ((result (call :|user_id| user-id)))
    (mapcar #'make-flickr-photoset (xml-children result))))

(defcall "photosets.getPhotos" (photoset-id)
  (let ((result (call :|photoset_id| photoset-id)))
    (mapcar #'make-flickr-photoset-photo (xml-children result))))

(defun deauthorize ()
  (setf *frob* nil)
  (setf *auth-token* nil)
  (setf *user* nil))

(defun request-authorization ()
  (deauthorize)
  (setf *frob* (auth-get-frob))
  (let* ((perms "write")
	 (api-sig (arguments-signature (list :|api_key| *api-key* :|perms| perms :|frob| *frob*)))
	 (url (format nil "http://flickr.com/services/auth/?api_key=~A&perms=~A&frob=~A&api_sig=~A"
		      *api-key* perms *frob* api-sig)))
    (run-program "open" (list url))
    url))

(defun complete-authorization ()
  (multiple-value-bind (token perms user)
      (auth-get-token)
    (setf *auth-token* token)
    (setf *user* user)
    perms))

(defun collect-pages (fetcher)
  (multiple-value-bind (list num-pages num-photos)
      (funcall fetcher 500 1)
    (declare (ignore num-photos))
    (if (> num-pages 1)
	(apply #'append list
	       (loop for page from 2 to num-pages
		     collect (funcall fetcher 500 page)))
	list)))
