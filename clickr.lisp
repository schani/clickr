(in-package :flickr)			;FIXME: make this clickr

(defvar *views-scanner* (cl-ppcre:create-scanner "Viewed <b>((\\d+)</b> times|once</b>)"))
(defvar *faves-scanner* (cl-ppcre:create-scanner "fave_count = (\\d+);</script>"))
(defvar *comment-scanner* (cl-ppcre:create-scanner "name=\"comment(\\d+)\"><img src=\"http://static.flickr.com/\\d+/buddyicons/(\\d+@\\w\\d+)[.]jpg"))

(defmacro defapiclass (name &key key sources fetchers custom-fetchers)
  (when (null key)
    (error "CLickr API Class ~A has no key" name))
  (labels ((accessor-method (class-name slot-name fetcher-name)
	     (let ((accessor-name (intern (format nil "~A-~A" class-name slot-name))))
	       `(defmethod ,accessor-name ((instance ,class-name))
		 ,(if (null fetcher-name)
		      '()
		      `(unless (slot-boundp instance ',slot-name)
			(,fetcher-name instance)))
		 (slot-value instance ',slot-name))))
	   (taker-name (struct-name)
	     (intern (format nil "TAKE-VALUES-FROM-~A" struct-name)))
	   (source-with-struct-name (struct-name)
	     (assoc struct-name sources))
	   (source-slots (source)
	     (destructuring-bind (struct-name slots &key custom)
		 source
	       (declare (ignore struct-name))
	       (apply #'append slots (mapcar #'car custom))))
	   (fetcher-name (struct-name)
	     (intern (format nil "FETCH-FROM-~A" struct-name)))
	   (fetcher-name-for-slot (slot-name)
	     (labels ((find-fetcher (fetchers)
			(if (null fetchers)
			    nil
			    (let* ((struct-name (caar fetchers))
				   (slots (source-slots (source-with-struct-name struct-name))))
			      (if (member slot-name slots)
				  (fetcher-name struct-name)
				  (find-fetcher (cdr fetchers)))))))
	       (find-fetcher fetchers))))
    (let* ((non-key-slots (reduce #'union (append (mapcar #'cadr sources)
						  (mapcar #'source-slots sources)
						  (mapcar #'car custom-fetchers))))
	   (slots (cons key non-key-slots))
	   (hash-table-name (intern (format nil "*~A-HASH-TABLE*" name)))
	   (constructor-name (intern (format nil "MAKE-~A" name))))
      `(progn
	(defvar ,hash-table-name (make-hash-table :test #'equal)) ;the hash table

	(defclass ,name ()		;the class
	  ((,key :accessor ,(intern (format nil "~A-~A" name key)))
	   ,@non-key-slots))

	(defun ,constructor-name (,key)	; the constructor
	  (let ((instance (gethash ,key ,hash-table-name)))
	    (when (null instance)
	      (setf instance (make-instance ',name))
	      (setf (slot-value instance ',key) ,key)
	      (setf (gethash ,key ,hash-table-name) instance))
	    instance))

	,@(mappend #'(lambda (source)
		       (destructuring-bind (struct-name source-slots &key custom)
			   source
			 `((defmethod ,(taker-name struct-name) ((instance ,name) struct)	;the takers
			     (with-slots ,source-slots instance
			       ,@(mapcar #'(lambda (slot) ;automatic slots
					     (let ((struct-slot-name (intern (format nil "~A-~A"
										     struct-name slot)
									     (find-package 'flickr))))
					       `(setf ,slot (,struct-slot-name struct))))
					 source-slots)
			       ,@(mapcar #'(lambda (custom-item) ;custom slots
					     `(funcall ,(cadr custom-item) instance struct))
					 custom)))
			   (defun ,(intern (format nil "MAKE-~A-FROM-~A" name struct-name)) (struct) ;the makers
			     (let ((instance (,constructor-name (,(intern (format nil "~A-~A"
										  struct-name key))
								  struct))))
			       (,(taker-name struct-name) instance struct)
			       instance)))))
		   sources)

	,@(mapcar #'(lambda (fetcher)	;the fetchers
		      (destructuring-bind (struct-name fetch)
			  fetcher
			(let ((fetcher-name (fetcher-name struct-name)))
			  `(defmethod ,fetcher-name ((instance ,name))
			    (with-slots ,slots instance
			      (,(taker-name struct-name) instance ,fetch))))))
		  fetchers)

	,@(mapcar #'(lambda (fetch-slot) ;the accessors
		      (let ((fetcher-name (fetcher-name-for-slot fetch-slot)))
			(accessor-method name fetch-slot fetcher-name)))
		  non-key-slots)

	,@(mappend #'(lambda (fetcher)	; the custom fetcher accessors
		       (destructuring-bind (fetch-slots fetcher-name)
			   fetcher
			 (mapcar #'(lambda (fetch-slot)
				     (accessor-method name fetch-slot fetcher-name))
				 fetch-slots)))
		   custom-fetchers)))))

(defapiclass user
    :key id
    :sources ((flickr-person
	       (username realname mbox-sha1sum location ispro))
	      (flickr-user
	       (username))
	      (flickr-contact
	       (username realname))
	      (flickr-public-contact
	       (username)))
    :fetchers ((flickr-person
		(people-get-info id)))
    :custom-fetchers (((photos)
		       fetch-user-photos)
		      ((photosets)
		       fetch-user-photosets)
		      ((groups)
		       fetch-user-groups)
		      ((contacts)
		       fetch-user-contacts)
		      ((favorites)
		       fetch-user-favorites)))

(defun user-with-name (name)
  (let* ((flickr-user (people-find-by-username name))
	 (user (make-user (flickr-user-id flickr-user))))
    (take-values-from-flickr-user user flickr-user)
    user))

(defapiclass note			;FIXME: add photo slot
    :key id
    :sources ((flickr-note
	       (x y w h text)
	       :custom (((author)
			 #'(lambda (instance flickr-note)
			     (setf (slot-value instance 'author)
				   (make-user (flickr-note-author flickr-note)))))))))

(defapiclass tag			;FIXME: add photo slot
    :key id
    :sources ((flickr-tag
	       (raw text)
	       :custom (((author)
			 #'(lambda (instance flickr-tag)
			     (setf (slot-value instance 'author)
				   (make-user (flickr-tag-author flickr-tag)))))))))

(defapiclass photo
    :key id
    :sources ((flickr-full-photo
	       (secret server isfavorite license rotation
		       title description
		       ispublic isfriend isfamily
		       posted taken takengranularity lastupdate
		       permcomment permaddmeta
		       cancomment canaddmeta
		       urls)
	       :custom (((owner notes tags)
			 #'(lambda (instance full-photo)
			     (setf (slot-value instance 'owner)
				   (make-user (flickr-full-photo-owner full-photo)))
			     (let ((notes (mapcar #'make-note-from-flickr-note
						  (flickr-full-photo-notes full-photo)))
				   (tags (mapcar #'make-tag-from-flickr-tag
						 (flickr-full-photo-tags full-photo))))
			       (setf (slot-value instance 'notes) notes)
			       (setf (slot-value instance 'tags) tags))))))
	      (flickr-search-photo
	       (secret server title
		      ispublic isfriend isfamily)
	       :custom (((owner)
			 #'(lambda (instance search-photo)
			     (setf (slot-value instance 'owner)
				   (make-user (flickr-search-photo-owner search-photo)))))))
	      (flickr-photoset-photo
	       (secret server title))
	      (flickr-favorite
	       (secret server title
		       ispublic isfriend isfamily)
	       :custom (((owner)
			 #'(lambda (instance favorite)
			     (setf (slot-value instance 'owner)
				   (make-user (flickr-favorite-owner favorite))))))))
    :fetchers ((flickr-full-photo
		(photos-get-info id))) ;FIXME: pass secret if available
    :custom-fetchers (((sets groups)
		       fetch-photo-contexts)
		      ((sizes)
		       fetch-photo-sizes)
		      ((num-views num-faves comments)
		       fetch-non-api-photo-stuff)))

(defapiclass photoset
    :key id
    :sources ((flickr-photoset
	       (primary secret server title description))
	      (flickr-photoset-info
	       (primary title description)
	       :custom (((owner)
			 #'(lambda (instance photoset-info)
			     (setf (slot-value instance 'owner)
				   (make-user (flickr-photoset-info-owner photoset-info)))))))
	      (flickr-context-set
	       (title)))
    :fetchers ((flickr-photoset-info
		(photosets-get-info id)))
    :custom-fetchers (((photos)
		       fetch-photoset-photos)))

(defapiclass group
    :key id
    :sources ((flickr-group
	       (title description privacy))
	      (flickr-list-group
	       (title))
	      (flickr-context-pool
	       (title)))
    :fetchers ((flickr-group
		(groups-get-info id)))
    :custom-fetchers (((photos)
		       fetch-group-photos)))

(defclass comment ()
  ((id :accessor comment-id :initarg :id)
   (photo :accessor comment-photo :initarg :photo)
   (sender :accessor comment-sender :initarg :sender)))

(defmethod fetch-group-photos ((group group))
  (let ((photos (collect-pages #'(lambda (per-page page)
				   (groups-pools-get-photos (group-id group)
							    :per-page per-page :page page)))))
    (setf (slot-value group 'photos)
	  (mapcar #'make-photo-from-flickr-search-photo photos))))

(defmethod fetch-user-photos ((user user))
  (let ((photos (collect-pages #'(lambda (per-page page)
				   (photos-search :user-id (user-id user)
						  :per-page per-page :page page)))))
    (setf (slot-value user 'photos)
	  (mapcar #'make-photo-from-flickr-search-photo photos))))

(defmethod fetch-user-photosets ((user user))
  (let ((photosets (photosets-get-list (user-id user))))
    (setf (slot-value user 'photosets)
	  (mapcar #'make-photoset-from-flickr-photoset photosets))))

(defmethod fetch-user-groups ((user user))
  (let ((groups (people-get-public-groups (user-id user))))
    (setf (slot-value user 'groups)
	  (mapcar #'make-group-from-flickr-list-group groups))))

(defmethod fetch-photoset-photos ((photoset photoset))
  (let ((photos (photosets-get-photos (photoset-id photoset))))
    (setf (slot-value photoset 'photos)
	  (mapcar #'make-photo-from-flickr-photoset-photo photos))))

(defmethod fetch-photo-contexts ((photo photo))
  (let ((contexts (photos-get-all-contexts (photo-id photo))))
    (setf (slot-value photo 'sets)
	  (mapcar #'(lambda (context-set)
		      (let ((set (make-photoset (flickr-context-set-id context-set))))
			(take-values-from-flickr-context-set set context-set)
			set))
		  (remove-if-not #'flickr-context-set-p contexts)))
    (setf (slot-value photo 'groups)
	  (mapcar #'(lambda (context-pool)
		      (let ((group (make-group (flickr-context-pool-id context-pool))))
			(take-values-from-flickr-context-pool group context-pool)
			group))
		  (remove-if-not #'flickr-context-pool-p contexts)))))

(defmethod fetch-user-contacts ((user user))
  (let ((contacts (contacts-get-public-list (user-id user))))
    (setf (slot-value user 'contacts)
	  (mapcar #'(lambda (contact)
		      (let ((contact-user (make-user (flickr-public-contact-id contact))))
			(take-values-from-flickr-public-contact contact-user contact)
			contact-user))
		  contacts))))

(defmethod fetch-user-favorites ((user user))
  (let ((favorites (collect-pages #'(lambda (per-page page)
				      (favorites-get-public-list (user-id user)
								 :per-page per-page :page page)))))
    (setf (slot-value user 'favorites)
	  (mapcar #'(lambda (favorite)
		      (let ((photo (make-photo (flickr-favorite-id favorite))))
			(take-values-from-flickr-favorite photo favorite)
			photo))
		  favorites))))

(defmethod fetch-photo-sizes ((photo photo))
  (setf (slot-value photo 'sizes) (photos-get-sizes (photo-id photo))))

(defmethod photo-photopage-url ((photo photo))
  (let ((urls (photo-urls photo)))
    (flickr-url-url (find-if #'(lambda (url) (string-equal "photopage" (flickr-url-type url))) urls))))

(defun read-url-response (url)
  (destructuring-bind (code headers stream)
      (http-get url)
    (declare (ignore code headers))
    (let ((lines nil))
      (do ((line (read-line stream nil nil) (read-line stream nil nil)))
	  ((null line))
	(push line lines))
      (apply #'concatenate 'string (reverse lines)))))

;; Returns number of views, number of faves and a list of comment
;; instances.  Comments by deleted users are not returned (yet).
(defmethod read-photo-counts ((photo photo))
  (let* ((url (photo-photopage-url photo))
	 (html (read-url-response url))
	 (num-views (multiple-value-bind (match regs)
			(cl-ppcre:scan-to-strings *views-scanner* html)
		      (declare (ignore match))
		      (if (null (aref regs 1))
			  1
			  (parse-integer (aref regs 1)))))
	 (num-faves (multiple-value-bind (match regs)
			(cl-ppcre:scan-to-strings *faves-scanner* html)
		      (declare (ignore match))
		      (parse-integer (aref regs 0))))
	 (comments nil))
    (cl-ppcre:do-scans (ms me rs re *comment-scanner* html)
      (let ((comment-id (subseq html (aref rs 0) (aref re 0)))
	    (sender-id (subseq html (aref rs 1) (aref re 1))))
	(push (make-instance 'comment
			     :id comment-id
			     :photo photo
			     :sender (make-user sender-id))
	      comments)))
    (values num-views
	    num-faves
	    (reverse comments))))

(defmethod fetch-non-api-photo-stuff ((photo photo))
  (multiple-value-bind (num-views num-faves comments)
      (read-photo-counts photo)
    (setf (slot-value photo 'num-views) num-views)
    (setf (slot-value photo 'num-faves) num-faves)
    (setf (slot-value photo 'comments) comments)))

(defun reset-clickr ()
  (setf *user-hash-table* (make-hash-table :test #'equal))
  (setf *note-hash-table* (make-hash-table :test #'equal))
  (setf *tag-hash-table* (make-hash-table :test #'equal))
  (setf *photo-hash-table* (make-hash-table :test #'equal))
  (setf *photoset-hash-table* (make-hash-table :test #'equal))
  (setf *group-hash-table* (make-hash-table :test #'equal)))

(defmethod add-tag ((photo photo) tags)
  (photos-add-tags (photo-id photo) tags)
  (slot-makunbound photo 'tags))

(defmethod add-photo ((photo photo) (set photoset))
  (photosets-add-photo (photoset-id set) (photo-id photo))
  (slot-makunbound photo 'sets)
  (slot-makunbound set 'photos))

(defmethod add-photo ((photo photo) (group group))
  (groups-pools-add (photo-id photo) (group-id group))
  (slot-makunbound photo 'groups)
  (slot-makunbound group 'photos))

(defmethod remove-photo ((photo photo) (group group))
  (groups-pools-remove (photo-id photo) (group-id group))
  (slot-makunbound photo 'groups)
  (slot-makunbound group 'photos))
