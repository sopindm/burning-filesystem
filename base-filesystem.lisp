(in-package :burning-filesystem)

(defgeneric filesystem= (fs1 fs2))
(defmethod filesystem= (fs1 fs2) (eq fs1 fs2))

(defstruct (path (:copier do-copy-path))
  root
  relative-p
  directory
  filename)

(defclass path-root ()
  ((filesystem :initarg :filesystem :initform nil :accessor path-root-filesystem)))

(defmethod print-object ((obj path-root) stream)
  (format stream "#<PATH-ROOT :filesystem ~a>" (path-root-filesystem obj)))

(defgeneric path-root= (root1 root2))

(defmethod path-root= ((root1 path-root) (root2 path-root))
  (filesystem= (path-root-filesystem root1) (path-root-filesystem root2)))

(defun copy-path-root (root)
  (if root (make-instance (type-of root) :default root)))

(defmethod initialize-instance :after ((obj path-root) &key default)
  (when default
    (setf (path-root-filesystem obj) (path-root-filesystem default))))

(defclass filename ()
  ((name :initarg :name :initform nil :accessor filename-name)
   (type :initarg :type :initform nil :accessor filename-type)))

(defmethod print-object ((obj filename) stream)
  (format stream "#<FILENAME :name ~a :type ~a>" (filename-name obj) (filename-type obj)))

(defgeneric filename= (name1 name2))

(defmethod filename= ((name1 (eql nil)) name2)
  (null name2))
(defmethod filename= (name1  (name2 (eql nil)))
  (null name1))

(defmethod filename= ((name1 filename) (name2 filename))
  (and (equal (filename-name name1) (filename-name name2))
       (equal (filename-type name1) (filename-type name2))))

(defun copy-filename (name)
  (if name (make-instance (type-of name) :default name)))

(defmethod initialize-instance :after ((obj filename) &key default)
  (when default
    (setf (filename-name obj) (filename-name default))
    (setf (filename-type obj) (filename-type default))))

(defun path= (path1 path2)
  (and (path-root= (path-root path1) (path-root path2))
       (eq (path-relative-p path1) (path-relative-p path2))
       (equal (path-directory path1) (path-directory path2))
       (filename= (path-filename path1) (path-filename path2))))

(defun %copy-path (path)
  (let ((new-path (do-copy-path path)))
    (setf (path-root new-path) (copy-path-root (path-root path)))
    (setf (path-filename new-path) (copy-filename (path-filename path)))
    new-path))

;;
;; Macro defining a generic and default implementation signaling error.
;;

(defmacro def-noimpl-generic (name (fs &rest arguments))
  (flet ((arguments-list (args)
	   (remove-if #'(lambda (x) (member x '(&key &optional &rest &allow-other-keys))) args)))
    `(progn 
       (defgeneric ,name ,(cons fs arguments))
       (defmethod ,name ,(cons fs arguments)
	 (declare (ignore ,@(arguments-list arguments)))
	 (flet ((fs-type (name)
		  (if (symbolp name)
		      name
		      (type-of name))))
	   (error "Method ~a isn't implemented for filesystem type ~a." ',name (fs-type ,fs)))))))

;;
;; Filesystem interface
;;

(def-noimpl-generic fs-path-from-string (fs string))
(def-noimpl-generic fs-path-to-string (fs path))

(defgeneric fs-make-filename (fs))
(defmethod fs-make-filename (fs)
  (when (null fs)
    (error "Internal error: null filesystem found"))
  (make-instance 'filename))

(def-noimpl-generic fs-file-exists-p (fs file))
(def-noimpl-generic fs-directory-exists-p (fs directory))

(def-noimpl-generic fs-list-directory (fs directory))

(def-noimpl-generic fs-current-directory (fs))
(def-noimpl-generic fs-home-directory (fs))

(def-noimpl-generic fs-delete-file (fs path))
(def-noimpl-generic fs-delete-directory (fs path))

(def-noimpl-generic fs-make-file (fs path))
(def-noimpl-generic fs-make-directory (fs path))

(def-noimpl-generic fs-as-file-path (fs directory-path))
(def-noimpl-generic fs-as-directory-path (fs file-path))

(defgeneric fs-open-file (fs path &key direction element-type if-exists if-does-not-exist))
(defgeneric fs-open-stream (fs path direction element-type position))

(def-noimpl-generic fs-file-length (fs path &optional element-type))

;;
;; Replaces existing file with new one
;;
;; Action is one of: :new-version, :rename, :rename-and-delete, :overwrite, :supersede

(defgeneric fs-file-replace (fs path action))

(defmethod fs-file-replace (fs path action)
  (ecase action
    ((:new-version :rename :rename-and-delete :supersede) (progn (fs-delete-file fs path)
								  (fs-make-file fs path)))
    (:overwrite t)))
  
;;
;; Error conditions
;;

(define-condition filesystem-error (error)
  ((path :initarg :path :reader filesystem-error-path)))

(define-condition file-lock-error (filesystem-error) ())

(define-condition file-does-not-exist-error (filesystem-error) ())
(define-condition directory-does-not-exist-error (filesystem-error) ())

(define-condition file-already-exists-error (filesystem-error) ())
(define-condition directory-already-exists-error (filesystem-error) ())

(define-condition no-correct-path-error (filesystem-error) ())   

(define-condition directory-not-empty-error (filesystem-error) ())

;;
;; Implementation
;;

(defmethod fs-open-file (fs path &key 
			 (direction :input) 
			 (element-type 'character) 
			 (if-exists :error)
			 (if-does-not-exist 'if-does-not-exist-default))
  (when (eq if-does-not-exist 'if-does-not-exist-default)
    (setf if-does-not-exist 
	  (ecase direction
	    (:input :error)
	    ((:output :io) (if (member if-exists '(:overwrite :append)) :error :create))
	    (:probe nil))))
  (let ((position :start))
    (labels ((check-if-exists ()
	       (ecase if-exists
		 (:error (error 'file-already-exists-error :path path))
		 ((:new-version :rename :rename-and-delete :overwrite :supersede) 
		  (fs-file-replace fs path if-exists))
		 (:append (setf position :end))
		 ((nil) nil)))
	     (check-if-does-not-exist ()
	       (ecase if-does-not-exist
		 (:error (error 'file-does-not-exist-error :path path))
		 (:create (fs-make-file fs path)
			  t)
		 ((nil) nil)))	       
	     (check-existance ()
	       (if (fs-file-exists-p fs path)
		   (or (member direction '(:input :probe))
		       (check-if-exists))
		   (check-if-does-not-exist))))
      (if (check-existance)
	  (fs-open-stream fs path direction element-type position)))))

(defgeneric fs-copy-file (fs source dest))

(defconstant *file-buffer-size* 4096)

(defun %copy-file (source dest)
  (let ((input (open-file source)))
    (unwind-protect
	 (let ((output (open-file dest :direction :output)))
	   (unwind-protect
		(let ((buffer (make-array *file-buffer-size* :element-type (stream-element-type input))))
		  (do ((size (read-sequence buffer input) (read-sequence buffer input)))
		      ((= size 0) nil)
		    (write-sequence buffer output :end size)))
	     (close output)))
      (close input))))

(defmethod fs-copy-file (fs source dest)
  (%copy-file (copy-path source :new-filesystem fs) (copy-path dest :new-filesystem fs)))

(defgeneric fs-copy-directory (fs source dest))

(defmethod fs-copy-directory (fs source dest)
  (fs-make-directory fs dest)
  (let ((entities (fs-list-directory fs source)))
    (mapc #'(lambda (x) (fs-copy-file fs x (path+ dest (path- x source)))) (remove-if-not #'file-path-p entities))
    (mapc #'(lambda (x) (fs-copy-directory fs x (path+ dest (path- x source))))
	  (remove-if-not #'directory-path-p entities))))

(defgeneric fs-move-file (fs source dest))

(defmethod fs-move-file (fs source dest)
  (fs-copy-file fs source dest)
  (fs-delete-file fs source))

(defgeneric fs-move-directory (fs source dest))

(defmethod fs-move-directory (fs source dest)
  (fs-copy-directory fs source dest)
  (fs-delete-directory fs source))


  

  