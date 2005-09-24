(in-package :flickr)

(defparameter *me* *schani*)

(defun text-for-raw-tag (raw)
  (string-downcase (remove-if #'(lambda (c) (find c " _-")) raw)))

(defstruct action
  name
  action
  condition)

(defparameter *actions* nil)

(defun add-action (name action condition)
  (let ((actions (remove name *actions* :key #'action-name)))
    (setf *actions* (cons (make-action :name name :action action :condition condition)
			  actions))))

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
  max-batch)

(defparameter *automatr-groups* nil)

(defmacro defgroup (name id &key max-posted max-batch)
  `(push (make-automatr-group :name ',name
			      :id ,id
			      :max-posted ,max-posted
			      :max-batch ,max-batch)
	 *automatr-groups*))

(defun automatr-group-with-name (name)
  (find name *automatr-groups* :key #'automatr-group-name))

(defun group-for-automatr-group (automatr-group)
  (make-group (automatr-group-id automatr-group)))

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
	  :func #'(lambda ,(mapcar #'car args) ,@body))
    *ops*))

(defun op-with-name (name)
  (find name *ops* :key #'op-name))

(defop count :integer ((list (:list ?)))
  (length list))

(defop / :number ((a :number) (b :number))
  (/ a b))

(defop >= :boolean ((a :number) (b :number))
  (>= a b))

(defop not :boolean ((x :boolean))
  (not x))

(defop and :boolean ((x :boolean) (y :boolean))
  (and x y))

(defop eq :boolean ((x ?t) (y ?t))
  (eq x y))

;; evaluates the expression for the given instance of an entity.
;; returns the result and the result's type
(defun eval-expr (expr instance entity expected-type)
  (declare (ignore expected-type))
  (cond ((eq expr '*me*)
	 (values *me* 'user))
	((symbolp expr)
	 (let ((slot (find expr (entity-slots entity) :key #'entity-slot-name)))
	   (assert (not (null slot)))
	   (values (funcall (entity-slot-accessor slot) instance)
		   (entity-slot-type slot))))
	((integerp expr)
	 (values expr :integer))
	((numberp expr)
	 (values expr :number))
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
		(values (apply (op-func op) args)
			(op-type op)))))))))

;; returns a list of all actions whose conditions are met by the
;; instance
(defun actions-for-instance (instance entity)
  (remove-if-not #'(lambda (action)
		     (eval-expr (action-condition action) instance entity :boolean))
		 *actions*))

(defun action-applicable-p (action photo)
  (case-match (action-action action)
    ((add-tag ?raw)
     (let ((text (text-for-raw-tag raw)))
       (not (member text (photo-tags photo) :key #'tag-text :test #'string-equal))))
    ((add-to-set ?id)
     (let ((set (make-photoset id)))
       (not (member photo (photoset-photos set)))))
    ((add-to-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (not (member photo (group-photos group)))))
    ((remove-from-group ?name)
     (let ((group (group-for-automatr-group-with-name name)))
       (member photo (group-photos group))))
    (?a
     (error "Unknown action ~A" a))))

(defun all-applicable-actions (user)
  (remove-if #'null
	     (mapcar #'(lambda (photo)
			 (cons photo
			       (remove-if-not #'(lambda (action)
						  (action-applicable-p action photo))
					      (actions-for-instance photo (entity-with-name 'photo)))))
		     (user-photos user))
	     :key #'cdr))

(defun apply-action (action photo)
  (case-match (action-action action)
    ((add-tag ?raw)
     (add-tag photo (list raw)))
    ((add-to-set ?id)
     (let ((set (make-photoset id)))
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
  (let* ((add-to-group-actions (remove-if-not #'(lambda (a) (matchp (action-action (cadr a)) (add-to-group ?)))
					      actions))
	 (group-names (mapcar #'(lambda (a)
				  (cadr (action-action (cadr a))))
			      add-to-group-actions))
	 (group-names (remove-duplicates group-names)))
    (reduce #'(lambda (actions group-name)
		(let ((group (automatr-group-with-name group-name)))
		  (if (null (automatr-group-max-batch group))
		      actions
		      (slet* ((group-actions other-actions
					     (partition #'(lambda (a)
							    (and (matchp (action-action (cadr a)) (add-to-group ?))
								 (eql (cadr (action-action (cadr a))) group-name)))
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
							       (list p (make-action :name 'remove-photo
										    :action `(remove-from-group 
											      ,(automatr-group-name group))
										    :condition 't)))
							   remove-photos)))
			      (append remove-actions audited-group-actions other-actions))
			    (append audited-group-actions other-actions))))))
	    group-names :initial-value actions)))

(defun apply-actions (actions)
  (dolist (a actions)
    (destructuring-bind (photo &rest actions)
	a
      (dolist (action actions)
	(apply-action action photo)))))

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
    ((sender #'comment-sender)))

(defgroup 1000views
    "83435940@N00")

(defgroup fav/view>=5%
    "39907031@N00"
  :max-batch 1)

(defgroup 100v+10f
    "72576714@N00"
  :max-batch 3)

(defgroup top-v
    "56022122@N00"
  :max-batch 1
  :max-posted 11)

(dolist (num '(111 333 555 777 999 1111 2222 3333 4444 5555 6666 7777 8888 9999))
  (add-action (intern (format nil "TOP-V~A" num))
	      `(add-tag ,(format nil "top-v~A" num))
	      `(>= num-views ,num)))

(add-action 'top-v
	    '(add-to-group top-v)
	    '(>= num-views 111))

(dolist (num '(25 50 75 100 150 200 250 300 350 400 450 500))
  (add-action (intern (format nil "TOP-F~A" num))
	      `(add-tag ,(format nil "top-f~A" num))
	      `(>= (count faves) ,num)))

(dolist (vals '((25 50) (51 75) (76 100) (101 150) (151 200) (201 250) (251 300)))
  (destructuring-bind (bound num)
      vals
    (add-action (intern (format nil "TOP-C~A" num))
		`(add-tag ,(format nil "top-c~A" num))
		`(>= (count (filter comments (not (eq sender *me*)))) ,bound))))

(add-action 'most-faved
	    '(add-to-set "487122")
	    '(>= (count faves) 6))

(add-action '1000views
	    '(add-to-group 1000views)
	    '(>= num-views 1000))

(add-action 'fav/view>=5%
	    '(add-to-group fav/view>=5%)
	    '(and (>= (count faves) 5) (>= (/ (count faves) num-views) 0.05)))

(add-action '100v+10f
	    '(add-to-group 100v+10f)
	    '(and (>= num-views 100) (>= (count faves) 10)))
