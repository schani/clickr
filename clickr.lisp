;;; clickr.lisp --- High-level Flickr interface

;; Copyright (C) 2006 Mark Probst

;; Author: Mark Probst <mark.probst@gmail.com>
;; Maintainer: Mark Probst <mark.probst@gmail.com>
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

(defpackage "CLICKR"
  (:use "CL" "TRIVIAL-HTTP" "FLICKR" "UTILS")
  (:export #:user-with-name #:user-photopage-url #:photo-photopage-url #:group-url #:reset-clickr
	   #:add-tag #:remove-tag #:has-tag #:add-photo #:remove-photo #:clickr-base-api-info #:contact-info
	   #:make-photo))

(in-package :clickr)

(defvar *views-scanner* (cl-ppcre:create-scanner "Viewed <b>((\\d+)</b> times|once</b>)"))
(defvar *faves-scanner* (cl-ppcre:create-scanner "fave_count = (\\d+);</script>"))

(defclass clickr-base ()
  ((api-info :accessor clickr-base-api-info :initarg :api-info)))

(defmacro defapiclass (name &key key sources fetchers custom-fetchers extra-slots)
  (when (null key)
    (error "CLickr API Class ~A has no key" name))
  (labels ((accessor-method (class-name slot-name fetcher-name)
	     (let ((accessor-name (intern (format nil "~A-~A" class-name slot-name))))
	       `(progn
		  (defmethod ,accessor-name ((instance ,class-name))
		    ,(if (null fetcher-name)
			 '()
			 `(unless (slot-boundp instance ',slot-name)
			    (,fetcher-name instance)))
		    (slot-value instance ',slot-name))
		  (export ',accessor-name))))
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
	   (constructor-name (intern (format nil "MAKE-~A" name)))
	   (key-accessor-name (intern (format nil "~A-~A" name key))))
      `(progn
	(defvar ,hash-table-name (make-hash-table :test #'equal)) ;the hash table

	(defclass ,name (clickr-base)	;the class
	  ((,key :accessor ,key-accessor-name)
	   ,@non-key-slots ,@extra-slots))
	(export ',key-accessor-name)

	(defun ,constructor-name (api-info ,key)	; the constructor
	  (let ((instance (gethash ,key ,hash-table-name)))
	    (when (null instance)
	      (setf instance (make-instance ',name))
	      (setf (slot-value instance 'api-info) api-info)
	      (setf (slot-value instance ',key) ,key)
	      (setf (gethash ,key ,hash-table-name) instance))
	    instance))
	(export ',constructor-name)

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
			   (defun ,(intern (format nil "MAKE-~A-FROM-~A" name struct-name)) (api-info struct) ;the makers
			     (let ((instance (,constructor-name api-info (,(intern (format nil "~A-~A"
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
		(people-get-info (clickr-base-api-info instance) id)))
    :custom-fetchers (((photos)
		       fetch-user-photos)
		      ((photosets)
		       fetch-user-photosets)
		      ((groups)
		       fetch-user-groups)
		      ((contacts)
		       fetch-user-contacts)
		      ((favorites)
		       fetch-user-favorites))
    :extra-slots (contact-list))

(defun user-with-name (api-info name)
  (let* ((flickr-user (people-find-by-username api-info name))
	 (user (make-user api-info (flickr-user-id flickr-user))))
    (take-values-from-flickr-user user flickr-user)
    user))

(defmethod user-photopage-url ((user user))
  (format nil "http://www.flickr.com/photos/~A" (user-id user)))

(defapiclass note			;FIXME: add photo slot
    :key id
    :sources ((flickr-note
	       (x y w h text)
	       :custom (((author)
			 #'(lambda (instance flickr-note)
			     (setf (slot-value instance 'author)
				   (make-user (clickr-base-api-info instance) (flickr-note-author flickr-note)))))))))

(defapiclass tag			;FIXME: add photo slot
    :key id
    :sources ((flickr-tag
	       (raw text)
	       :custom (((author)
			 #'(lambda (instance flickr-tag)
			     (setf (slot-value instance 'author)
				   (make-user (clickr-base-api-info instance) (flickr-tag-author flickr-tag)))))))))

(defapiclass comment			;FIXME: add photo slot
    :key id
    :sources ((flickr-comment
	       (date-create permalink text)
	       :custom (((author)
			 #'(lambda (instance flickr-comment)
			     (setf (slot-value instance 'author)
				   (make-user (clickr-base-api-info instance) (flickr-comment-author flickr-comment)))))))))

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
				   (make-user (clickr-base-api-info instance) (flickr-full-photo-owner full-photo)))
			     (let ((notes (mapcar #'(lambda (x) (make-note-from-flickr-note (clickr-base-api-info instance) x))
						  (flickr-full-photo-notes full-photo)))
				   (tags (mapcar #'(lambda (x) (make-tag-from-flickr-tag (clickr-base-api-info instance) x))
						 (flickr-full-photo-tags full-photo))))
			       (setf (slot-value instance 'notes) notes)
			       (setf (slot-value instance 'tags) tags))))))
	      (flickr-search-photo
	       (secret server title
		      ispublic isfriend isfamily)
	       :custom (((owner)
			 #'(lambda (instance search-photo)
			     (setf (slot-value instance 'owner)
				   (make-user (clickr-base-api-info instance) (flickr-search-photo-owner search-photo)))))))
	      (flickr-photoset-photo
	       (secret server title))
	      (flickr-favorite
	       (secret server title
		       ispublic isfriend isfamily)
	       :custom (((owner)
			 #'(lambda (instance favorite)
			     (setf (slot-value instance 'owner)
				   (make-user (clickr-base-api-info instance) (flickr-favorite-owner favorite))))))))
    :fetchers ((flickr-full-photo
		(photos-get-info (clickr-base-api-info instance) id))) ;FIXME: pass secret if available
    :custom-fetchers (((sets groups)
		       fetch-photo-contexts)
		      ((sizes)
		       fetch-photo-sizes)
		      ((comments)
		       fetch-photo-comments)
		      ((num-views num-faves)
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
				   (make-user (clickr-base-api-info instance) (flickr-photoset-info-owner photoset-info)))))))
	      (flickr-context-set
	       (title)))
    :fetchers ((flickr-photoset-info
		(photosets-get-info (clickr-base-api-info instance) id)))
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
		(groups-get-info (clickr-base-api-info instance) id)))
    :custom-fetchers (((photos)
		       fetch-group-photos)))

(defmethod fetch-group-photos ((group group))
  (let ((photos (collect-pages #'(lambda (per-page page)
				   (groups-pools-get-photos (clickr-base-api-info group) (group-id group)
							    :per-page per-page :page page)))))
    (setf (slot-value group 'photos)
	  (mapcar #'(lambda (x) (make-photo-from-flickr-search-photo (clickr-base-api-info group) x))
		  photos))))

(defmethod fetch-user-photos ((user user))
  (let ((photos (collect-pages #'(lambda (per-page page)
				   (photos-search (clickr-base-api-info user) :user-id (user-id user)
						  :per-page per-page :page page)))))
    (setf (slot-value user 'photos)
	  (mapcar #'(lambda (x) (make-photo-from-flickr-search-photo (clickr-base-api-info user) x))
		  photos))))

(defmethod fetch-user-photosets ((user user))
  (let ((photosets (photosets-get-list (clickr-base-api-info user) (user-id user))))
    (setf (slot-value user 'photosets)
	  (mapcar #'(lambda (x) (make-photoset-from-flickr-photoset (clickr-base-api-info user) x))
		  photosets))))

(defmethod fetch-user-groups ((user user))
  (let ((groups (people-get-public-groups (clickr-base-api-info user) (user-id user))))
    (setf (slot-value user 'groups)
	  (mapcar #'(lambda (x) (make-group-from-flickr-list-group (clickr-base-api-info user) x))
		  groups))))

(defmethod fetch-photoset-photos ((photoset photoset))
  (let ((photos (photosets-get-photos (clickr-base-api-info photoset) (photoset-id photoset))))
    (setf (slot-value photoset 'photos)
	  (mapcar #'(lambda (x) (make-photo-from-flickr-photoset-photo (clickr-base-api-info photoset) x))
		  photos))))

(defmethod fetch-photo-contexts ((photo photo))
  (let ((contexts (photos-get-all-contexts (clickr-base-api-info photo) (photo-id photo))))
    (setf (slot-value photo 'sets)
	  (mapcar #'(lambda (context-set)
		      (let ((set (make-photoset (clickr-base-api-info photo) (flickr-context-set-id context-set))))
			(take-values-from-flickr-context-set set context-set)
			set))
		  (remove-if-not #'flickr-context-set-p contexts)))
    (setf (slot-value photo 'groups)
	  (mapcar #'(lambda (context-pool)
		      (let ((group (make-group (clickr-base-api-info photo) (flickr-context-pool-id context-pool))))
			(take-values-from-flickr-context-pool group context-pool)
			group))
		  (remove-if-not #'flickr-context-pool-p contexts)))))

(defmethod fetch-user-contacts ((user user))
  (let ((contacts (contacts-get-public-list (clickr-base-api-info user) (user-id user))))
    (setf (slot-value user 'contacts)
	  (mapcar #'(lambda (contact)
		      (let ((contact-user (make-user (clickr-base-api-info user) (flickr-public-contact-id contact))))
			(take-values-from-flickr-public-contact contact-user contact)
			contact-user))
		  contacts))))

(defmethod fetch-user-favorites ((user user))
  (let ((favorites (collect-pages #'(lambda (per-page page)
				      (favorites-get-public-list (clickr-base-api-info user) (user-id user)
								 :per-page per-page :page page)))))
    (setf (slot-value user 'favorites)
	  (mapcar #'(lambda (favorite)
		      (let ((photo (make-photo (clickr-base-api-info user) (flickr-favorite-id favorite))))
			(take-values-from-flickr-favorite photo favorite)
			photo))
		  favorites))))

(defmethod fetch-photo-sizes ((photo photo))
  (setf (slot-value photo 'sizes) (photos-get-sizes (clickr-base-api-info photo) (photo-id photo))))

(defmethod fetch-photo-comments ((photo photo))
  (let ((comments (photos-comments-get-list (clickr-base-api-info photo) (photo-id photo))))
    (setf (slot-value photo 'comments)
	  (mapcar #'(lambda (x) (make-comment-from-flickr-comment (clickr-base-api-info photo) x)) comments))))

(defmethod photo-photopage-url ((photo photo))
  (let ((urls (photo-urls photo)))
    (flickr-url-url (find-if #'(lambda (url) (string-equal "photopage" (flickr-url-type url))) urls))))

(defmethod group-url ((group group))
  (format nil "http://www.flickr.com/groups/~A" (group-id group)))

(defun read-url-response (url)
  (destructuring-bind (code headers stream)
      (http-get url)
    (declare (ignore code headers))
    (let ((lines nil))
      (do ((line (read-line stream nil nil) (read-line stream nil nil)))
	  ((null line))
	(push line lines))
      (close stream)
      (apply #'concatenate 'string (reverse lines)))))

;; Returns number of views and number of faves.
(defmethod read-photo-counts ((photo photo))
  (let* ((url (photo-photopage-url photo))
	 (html (read-url-response url))
	 (num-views (multiple-value-bind (match regs)
			(cl-ppcre:scan-to-strings *views-scanner* html)
		      (declare (ignore match))
		      (cond ((null regs)
			     0)
			    ((null (aref regs 1))
			     1)
			    (t
			     (parse-integer (aref regs 1))))))
	 (num-faves (multiple-value-bind (match regs)
			(cl-ppcre:scan-to-strings *faves-scanner* html)
		      (declare (ignore match))
		      (if (null regs)
			  0
			  (parse-integer (aref regs 0))))))
    (values num-views
	    num-faves)))

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

(defun text-for-raw-tag (raw)
  (string-downcase (remove-if #'(lambda (c) (find c " _-")) raw)))

(defmethod add-tag ((photo photo) tags)
  (photos-add-tags (clickr-base-api-info photo) (photo-id photo) tags)
  (slot-makunbound photo 'tags))

(defmethod remove-tag ((photo photo) (tag tag))
  (assert (member tag (photo-tags photo)))
  (photos-remove-tag (clickr-base-api-info photo) (tag-id tag))
  (slot-makunbound photo 'tags))

(defmethod remove-tag ((photo photo) (raw string))
  (let* ((text (text-for-raw-tag raw))
	 (tag (find text (photo-tags photo) :key #'tag-text :test #'string-equal)))
    (if (null tag)
	(error "Photo has no tag ~A" raw)
	(remove-tag photo tag))))

(defmethod has-tag ((photo photo) (raw string))
  (let ((text (text-for-raw-tag raw)))
    (member text (photo-tags photo) :key #'tag-text :test #'string-equal)))

(defmethod add-photo ((photo photo) (set photoset))
  (photosets-add-photo (clickr-base-api-info photo) (photoset-id set) (photo-id photo))
  (slot-makunbound photo 'sets)
  (slot-makunbound set 'photos))

(defmethod add-photo ((photo photo) (group group))
  (ignore-errors (groups-pools-add (clickr-base-api-info photo) (photo-id photo) (group-id group)))
  (slot-makunbound photo 'groups)
  (slot-makunbound group 'photos))

(defmethod remove-photo ((photo photo) (group group))
  (ignore-errors (groups-pools-remove (clickr-base-api-info photo) (photo-id photo) (group-id group)))
  (slot-makunbound photo 'groups)
  (slot-makunbound group 'photos))

(defmethod contact-info ((user user) (contact user))
  (unless (string= (user-id user) (flickr-user-id (flickr-api-info-user (clickr-base-api-info user))))
    (error "Cannot get contact info for user ~A" (user-id user)))
  (unless (slot-boundp user 'contact-list)
    (setf (slot-value user 'contact-list) (contacts-get-list (clickr-base-api-info user))))
  (let ((flickr-contact (find (user-id contact) (slot-value user 'contact-list) :key #'flickr-contact-id :test #'string=)))
    (unless flickr-contact
      (error "Have no contact ~A" (user-id contact)))
    (append (if (flickr-contact-isfriend flickr-contact) '(:isfriend) nil)
	    (if (flickr-contact-isfamily flickr-contact) '(:isfamily) nil)
	    (if (flickr-contact-ignored flickr-contact) '(:ignored) nil))))
