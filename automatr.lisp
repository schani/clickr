;;; automatr.lisp --- Automation of Flickr tasks

;; Copyright (C) 2006 Mark Probst

;; Author: Mark Probst <mark.probst@gmail.com>
;; Maintainer: Mark Probst <mark.probst@gmail.com>
;; Version: 0.1

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

(defpackage "AUTOMATR"
  (:use "CL" "FLICKR" "CLICKR" "UTILS" "LET-MATCH")
  (:export #:htmlize-actions #:all-applicable-actions #:audit-actions #:apply-actions #:*me*))

(in-package :automatr)

(defparameter *me* nil)
(proclaim '(special *me*))

(defstruct action
  name
  action
  condition)

(defparameter *actions* nil)

(defun add-action (name action condition)
  (let ((actions (remove name *actions* :key #'action-name)))
    (setf *actions* (cons (make-action :name name :action action :condition condition)
			  actions))))

(defun add-state-tag-actions (name tag-name condition)
  (add-action (intern (format nil "ADD-~A" name))
	      `(add-tag ,tag-name)
	      condition)
  (add-action (intern (format nil "REMOVE-~A" name))
	      `(remove-tag ,tag-name)
	      `(not ,condition)))

(defstruct entity
  name
  slots)

(defstruct entity-slot
  name
  accessor
  type)

(defparameter *entities* nil)

(defmacro defentity (name slots)
  `(push (make-entity :name ',name
		      :slots (list ,@(mapcar #'(lambda (slot)
						 (destructuring-bind (name accessor &optional (type :string))
						     slot
						   `(make-entity-slot :name ',name
								      :accessor ,accessor
								      :type ',type)))
					     slots)))
    *entities*))

(defun entity-name-p (name)
  (member name *entities* :key #'entity-name))

(defun entity-with-name (name)
  (find name *entities* :key #'entity-name))

(defstruct automatr-group
  name
  id
  max-posted
  max-batch
  condition)

(defparameter *automatr-groups* nil)

(defun defgroup (name id &key max-posted max-batch condition)
  (push (make-automatr-group :name name
			     :id id
			     :max-posted max-posted
			     :max-batch max-batch
			     :condition condition)
	*automatr-groups*))

(defun automatr-group-with-name (name)
  (find name *automatr-groups* :key #'automatr-group-name))

(defun group-for-automatr-group (automatr-group)
  (make-group (clickr-base-api-info *me*) (automatr-group-id automatr-group)))

(defun group-for-automatr-group-with-name (name)
  (let ((automatr-group (automatr-group-with-name name)))
    (group-for-automatr-group automatr-group)))

(defstruct op
  name
  type
  arg-types
  func)

(defparameter *ops* nil)

(defmacro defop (name type args &body body)
  `(push (make-op :name ',name
	  :type ',type
	  :arg-types ',(mapcar #'cadr args)
	  :func #'(lambda (instance ,@(mapcar #'car args)) ,@body))
    *ops*))

(defun op-with-name (name)
  (find name *ops* :key #'op-name))

(defop count :integer ((list (:list ?)))
  (length list))

(defop / :number ((a :number) (b :number))
  (if (zerop b)
      0
      (/ a b)))

(defop >= :boolean ((a :number) (b :number))
  (>= a b))

(defop not :boolean ((x :boolean))
  (not x))

(defop and :boolean ((x :boolean) (y :boolean))
  (and x y))

(defop or :boolean ((x :boolean) (y :boolean))
  (or x y))

(defop eq :boolean ((x ?t) (y ?t))
  (eq x y))

(defop in-set :boolean ((id :string))
  (let ((set (make-photoset (clickr-base-api-info *me*) id)))
    (member instance (photoset-photos set))))

(defun eval-expr (expr instance entity expected-type)
  "Evaluates the expression for the given instance of an entity.
   Returns the result and the result's type."
  (declare (ignore expected-type))
  (cond ((eq expr '*me*)
	 (values *me* 'user))
	((eq expr 't)
	 (values t :boolean))
	((symbolp expr)
	 (let ((slot (find expr (entity-slots entity) :key #'entity-slot-name)))
	   (assert (not (null slot)))
	   (values (funcall (entity-slot-accessor slot) instance)
		   (entity-slot-type slot))))
	((integerp expr)
	 (values expr :integer))
	((numberp expr)
	 (values expr :number))
	((stringp expr)
	 (values expr :string))
	((listp expr)
	 (case (car expr)
	   (filter
	    (destructuring-bind (list-expr condition-expr)
		(cdr expr)
	      (multiple-value-bind (list list-type)
		  (eval-expr list-expr instance entity '(:list ?))
		(assert (and (listp list-type) (eq (car list-type) :list)))
		(let* ((element-type (cadr list-type))
		       (is-entity (entity-name-p element-type))
		       (entity (if is-entity (entity-with-name element-type) entity)))
		  (remove-if-not #'(lambda (x)
				     (eval-expr condition-expr
						(if is-entity x instance)
						entity
						:boolean))
				 list)))))
	   (t
	    (let ((op (op-with-name (car expr))))
	      (assert (not (null op)))
	      (assert (= (length (cdr expr)) (length (op-arg-types op))))
	      (let ((args (mapcar #'(lambda (arg-expr arg-type)
				      (eval-expr arg-expr instance entity arg-type))
				  (cdr expr) (op-arg-types op))))
		(values (apply (op-func op) instance args)
			(op-type op)))))))
	(t
	 (error "illegal expression ~A" expr))))

(defun actions-for-instance (instance entity)
  "Returns a list of all actions whose conditions are met by the
   instance."
  (remove-if-not #'(lambda (action)
		     (eval-expr (action-condition action) instance entity :boolean))
		 *actions*))

(defun removal-action-for-automatr-group (automatr-group)
  (let ((name (automatr-group-name automatr-group)))
    (make-action :name (intern (format nil "REMOVE-FROM-~A" name))
		 :action `(remove-from-group ,name)
		 :condition 't)))

(defun group-removals-for-photo (photo)
  (mappend #'(lambda (automatr-group)
	       (if (and (not (null (automatr-group-condition automatr-group)))
			(member photo (group-photos (group-for-automatr-group automatr-group)))
			(not (eval-expr (automatr-group-condition automatr-group)
					photo
					(entity-with-name 'photo)
					:boolean)))
		   (list (removal-action-for-automatr-group automatr-group))
		   nil))
	   *automatr-groups*))

(defun group-condition-satisfied (group photo)
  (or (null (automatr-group-condition group))
      (eval-expr (automatr-group-condition group)
		 photo
		 (entity-with-name 'photo)
		 :boolean)))

(defun action-applicable-p (action photo)
  (case-match (action-action action)
    ((add-tag ?raw)
     (not (has-tag photo raw)))
    ((remove-tag ?raw)
     (has-tag photo raw))
    ((add-to-set ?id)
     (let ((set (make-photoset (clickr-base-api-info *me*) id)))
       (not (member photo (photoset-photos set)))))
    ((add-to-group ?name)
     (let* ((automatr-group (automatr-group-with-name name))
	    (group (group-for-automatr-group automatr-group)))
       (and (not (member photo (group-photos group)))
	    (group-condition-satisfied automatr-group photo))))
    ((remove-from-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (member photo (group-photos group))))
    (?a
     (error "Unknown action ~A" a))))

(defun all-applicable-actions (user)
  (remove-if #'null
	     (mappend #'(lambda (photo)
			  (let* ((actions (actions-for-instance photo (entity-with-name 'photo)))
				 (applicable-actions (remove-if-not #'(lambda (action)
									(action-applicable-p action photo))
								    actions))
				 (group-removals (group-removals-for-photo photo)))
			    (mapcar #'(lambda (a) (cons photo a))
				    (append applicable-actions group-removals))))
		      (user-photos user))
	     :key #'cdr))

(defun apply-action (action photo)
  (case-match (action-action action)
    ((add-tag ?raw)
     (add-tag photo (list raw)))
    ((remove-tag ?raw)
     (remove-tag photo raw))
    ((add-to-set ?id)
     (let ((set (make-photoset (clickr-base-api-info *me*) id)))
       (add-photo photo set)))
    ((add-to-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (add-photo photo group)))
    ((remove-from-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (remove-photo photo group)))
    (?a
     (error "Unknown action ~A" a))))

(defun audit-actions (actions)
  (let* ((add-to-group-actions (remove-if-not #'(lambda (a) (matchp (action-action (cdr a)) (add-to-group ?)))
					      actions))
	 (group-names (mapcar #'(lambda (a)
				  (cadr (action-action (cdr a))))
			      add-to-group-actions))
	 (group-names (remove-duplicates group-names)))
    (reduce #'(lambda (actions group-name)
		(let ((group (automatr-group-with-name group-name)))
		  (if (null (automatr-group-max-batch group))
		      actions
		      (slet* ((group-actions other-actions
					     (partition #'(lambda (a)
							    (and (matchp (action-action (cdr a)) (add-to-group ?))
								 (eql (cadr (action-action (cdr a))) group-name)))
							actions))
			      (audited-group-actions (random-select (automatr-group-max-batch group) group-actions)))
			(if (not (null (automatr-group-max-posted group)))
			    (let* ((clickr-group (group-for-automatr-group group))
				   (user-group-photos (remove-if-not #'(lambda (p)
									 (eq *me* (photo-owner p)))
								     (group-photos clickr-group)))
				   (num-remove (max (- (+ (length user-group-photos) (length audited-group-actions))
						       (automatr-group-max-posted group))
						    0))
				   (remove-photos (random-select num-remove user-group-photos))
				   (remove-actions (mapcar #'(lambda (p)
							       (cons p (removal-action-for-automatr-group group)))
							   remove-photos)))
			      (append remove-actions audited-group-actions other-actions))
			    (append audited-group-actions other-actions))))))
	    group-names :initial-value actions)))

(defun apply-actions (actions)
  (dolist (a actions)
    (destructuring-bind (photo . action)
	a
      (apply-action action photo))))

(defun action-html (action)
  (case-match (action-action action)
    ((add-tag ?raw)
     (format nil "Add tag ~A" raw))
    ((remove-tag ?raw)
     (format nil "Remove tag ~A" raw))
    ((add-to-set ?id)
     (let ((set (make-photoset (clickr-base-api-info *me*) id)))
       (format nil "Add to set ~A" (photoset-title set))))
    ((add-to-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (format nil "Add to group ~A" (group-title group))))
    ((remove-from-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (format nil "Remove from group ~A" (group-title group))))
    (?a
     (format nil "Unknown action ~A" a))))

(defun htmlize-actions (actions)
  (with-open-file (out "actions.html" :direction :output :if-exists :supersede)
    (format out "<html><body><table>~%")
    (dolist (a actions)
      (destructuring-bind (photo . action)
	  a
	(format out "<tr><td><a href=\"~A\"><img src=\"http://static.flickr.com/~A/~A_~A_t.jpg\"></a><td>~A<td>~A~%"
		(photo-photopage-url photo)
		(photo-server photo)
		(photo-id photo)
		(photo-secret photo)
		(action-name action)
		(action-html action))))
    (format out "</table></body></html>~%")))

(defentity photo
    ((isfavorite #'photo-isfavorite :boolean)
     (license #'photo-license)
     (ispublic #'photo-ispublic :boolean)
     (isfriend #'photo-isfriend :boolean)
     (isfamily #'photo-isfamily :boolean)
     (owner #'photo-owner user)
     (notes #'photo-notes (:list note))
     (tags #'photo-tags (:list tag))
     (sets #'photo-sets (:list set))
     (groups #'photo-groups (:list group))
     (sizes #'photo-sizes (:list size))
     (num-views #'photo-num-views :integer)
     (faves #'(lambda (photo)
		(integers-upto (photo-num-faves photo)))
	    (:list fave))
     (comments #'photo-comments (:list comment))))

(defentity user
    ((username #'user-username)
     (realname #'user-realname)
     (location #'user-location)
     (photos #'user-photos (:list photo))
     (sets #'user-photosets (:list set))
     (groups #'user-groups (:list group))
     (contacts #'user-contacts (:list user))
     (faves #'user-favorites (:list photo))))

(defentity set
    ((title #'photoset-title)
     (photos #'photoset-photos (:list photo))))

(defentity group
    ((title #'group-title)
     (photos #'group-photos (:list photo))))

(defentity note
    ())

(defentity tag
    ((raw #'tag-raw)
     (text #'tag-text)))

(defentity comment
    ((author #'comment-author)))

(defgroup '1000views
    "83435940@N00")

(defgroup 'fav/view>=5%
    "39907031@N00"
  :max-batch 1)

(defgroup '100v+10f
    "72576714@N00"
  :max-batch 3)

(defgroup '1000v+100f
    "79664037@N00"
  :max-batch 1)

(defgroup '1000v+40f
    "47981347@N00"
  :max-batch 1)

(defgroup '3000v+120f
    "95324234@N00"
  :max-batch 1)

(defgroup 'centurian
    "38475367@N00"
  :max-batch 3)

(defgroup '200v
    "23348528@N00"
  :max-batch 3)

(defgroup '500v+50f
    "49764889@N00"
  :max-batch 1)

(defgroup '500v+20f
    "42378300@N00"
  :max-batch 1)

(defgroup '2000v
    "74001424@N00"
  :max-batch 1)

(defgroup 'fave30times
    "22944948@N00"
  :max-batch 1)

(defgroup '50faves
    "41075027@N00"
  :max-batch 1)

(defgroup '100faves
    "28795624@N00"
  :max-batch 1)

(defgroup '100club
    "11512103@N00"
  :max-batch 1)

(defgroup 'top-v
    "56022122@N00"
  :max-batch 1
  :max-posted 11)

(defgroup 'top-f
    "23561440@N00"
  :max-batch 1)

(defgroup 'showcase
    "62795763@N00"
  :max-batch 1
  :max-posted 5)

(defgroup 'noteworthy-notes
    "19704745@N00"
  :max-batch 1)

(defgroup 'iflickr
    "30845071@N00"
  :max-batch 1
  :max-posted 5)

(defgroup 'flickr-addicts
    "76535076@N00"
  :max-batch 1
  :max-posted 5)

(defgroup 'flickr-central
    "34427469792@N01"
  :max-batch 1
  :max-posted 5)

(defgroup '1-to-5-favorites
    "71394080@N00"
  :max-batch 1
  :max-posted 10
  :condition '(and (>= (count faves) 1) (>= 5 (count faves))))

(defgroup '5-to-10-favorites
    "28531369@N00"
  :max-batch 1
  :max-posted 10
  :condition '(and (>= (count faves) 5) (>= 10 (count faves))))

(defgroup '10-to-25-favorites
    "39445275@N00"
  :max-batch 1
  :max-posted 10
  :condition '(and (>= (count faves) 10) (>= 25 (count faves))))

(dolist (group '((11 1111 "10021501@N00")
		 (22 2222 "51639810@N00")
		 (33 3333 "93348988@N00")
		 (44 4444 "13466285@N00")
		 (55 5555 "63242381@N00")
		 (77 7777 "66069685@N00")
		 (88 8888 "82243602@N00")
		 (99 9999 "63114470@N00")))
  nil)

(dolist (num '(111 333 555 777 999 1111 2222 3333 4444 5555 6666 7777 8888 9999))
  (add-action (intern (format nil "TOP-V~A" num))
	      `(add-tag ,(format nil "top-v~A" num))
	      `(>= num-views ,num)))

(add-action 'top-v
	    '(add-to-group top-v)
	    '(>= num-views 111))

(add-action 'top-f
	    '(add-to-group top-f)
	    '(>= (count faves) 25))

(dolist (num '(25 50 75 100 150 200 250 300 350 400 450 500))
  (add-action (intern (format nil "TOP-F~A" num))
	      `(add-tag ,(format nil "top-f~A" num))
	      `(>= (count faves) ,num)))

(dolist (vals '((25 50) (51 75) (76 100) (101 150) (151 200) (201 250) (251 300)))
  (destructuring-bind (bound num)
      vals
    (add-action (intern (format nil "TOP-C~A" num))
		`(add-tag ,(format nil "top-c~A" num))
		`(>= (count (filter comments (not (eq author *me*)))) ,bound))))

(add-action 'most-faved
	    '(add-to-set "487122")
	    '(>= (count faves) 30))

(add-action '1000views
	    '(add-to-group 1000views)
	    '(>= num-views 1000))

(add-action 'fav/view>=5%
	    '(add-to-group fav/view>=5%)
	    '(and (>= (count faves) 5) (>= (/ (count faves) num-views) 0.05)))

(add-action '100v+10f
	    '(add-to-group 100v+10f)
	    '(and (>= num-views 100) (>= (count faves) 10)))

(add-action '1000v+40f
	    '(add-to-group 1000v+40f)
	    '(and (and (>= num-views 1000) (>= 1500 num-views)) (>= (count faves) 40)))

(add-action '1000v+40f-tag
	    '(add-tag "1000v40f")
	    '(and (and (>= num-views 1000) (>= 1500 num-views)) (>= (count faves) 40)))

(add-action '1000v+100f
	    '(add-to-group 1000v+100f)
	    '(and (>= num-views 1000) (>= (count faves) 100)))

(add-action '3000v+120f
	    '(add-to-group 3000v+120f)
	    '(and (>= num-views 3000) (>= (count faves) 120)))

(add-action 'centurian
	    '(add-to-group centurian)
	    '(and (>= num-views 100) (>= 190 num-views)))

(add-action '200v
	    '(add-to-group 200v)
	    '(>= num-views 200))

(add-action '500v+50f
	    '(add-to-group 500v+50f)
	    '(and (>= num-views 500) (>= (count faves) 50)))

(add-action '500v+20f
	    '(add-to-group 500v+20f)
	    '(and (and (>= num-views 500) (>= 1000 num-views))
	          (>= (count faves) 20)))

(add-action '500v+20f-tag
	    '(add-tag "500v20f")
	    '(and (and (>= num-views 500) (>= 1000 num-views))
	          (>= (count faves) 20)))

(add-action '2000v
	    '(add-to-group 2000v)
	    '(>= num-views 2000))

(add-action 'fave30times
	    '(add-to-group fave30times)
	    '(>= (count faves) 30))

(add-action '50faves
	    '(add-to-group 50faves)
	    '(>= (count faves) 50))

(add-action '100faves
	    '(add-to-group 100faves)
	    '(>= (count faves) 100))

(add-action '100club
	    '(add-to-group 100club)
	    '(>= (count faves) 100))

(add-action '1-to-5-favorites
	    '(add-to-group 1-to-5-favorites)
	    't)

(add-action '5-to-10-favorites
	    '(add-to-group 5-to-10-favorites)
	    't)

(add-action '10-to-25-favorites
	    '(add-to-group 10-to-25-favorites)
	    't)

(add-action 'showcase
	    '(add-to-group showcase)
	    '(in-set "292480"))

(add-action 'noteworthy-notes
	    '(add-to-group noteworthy-notes)
	    '(>= (count notes) 1))

(add-state-tag-actions '1-5-fav
		       "1-5-fav"
		       '(and (>= (count faves) 1) (>= 5 (count faves))))

(add-state-tag-actions '5-10-fav
		       "5-10-fav"
		       '(and (>= (count faves) 5) (>= 10 (count faves))))

(add-state-tag-actions '10-25-fav
		       "10-25-fav"
		       '(and (>= (count faves) 10) (>= 25 (count faves))))

;(add-action 'iflickr
;	    '(add-to-group iflickr)
;	    '(or (in-set "292480") (>= (count faves) 10)))

;(add-action 'flickr-addicts
;	    '(add-to-group flickr-addicts)
;	    '(or (in-set "292480") (>= (count faves) 10)))

;(add-action 'flickr-central
;	    '(add-to-group flickr-central)
;	    '(or (in-set "292480") (>= (count faves) 10)))
