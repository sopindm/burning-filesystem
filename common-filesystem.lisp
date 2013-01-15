(in-package :burning-filesystem)

;;
;; Pathnames
;;

(defun %component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun %directory-p (path)
  (and (not (%component-present-p (cl:pathname-name path)))
       (not (%component-present-p (cl:pathname-type path)))))

(defun %pathname-as-directory (pathname)
  (if (%directory-p pathname)
      pathname
      (cl:make-pathname :directory (append (or (cl:pathname-directory pathname) '(:relative))
					   (list (cl:file-namestring pathname)))
			:name nil
			:type nil
			:defaults pathname)))

(defun %directory-wildcard (pathname)
  (cl:make-pathname :name :wild
		    :type #-clisp :wild #+clisp nil
		    :defaults (%pathname-as-directory pathname)))

#+clisp
(defun %subdirectories-wildcard (pathname)
  (cl:make-pathname :directory (append (cl:pathname-directory pathname) '(:wild))
		    :name nil
		    :type nil
		    :defaults pathname))

(defun %file-exists-p (pathname)
  #+(or sbcl ccl)
  (cl:probe-file pathname)
  #+clisp
  (or (ignore-errors (cl:probe-file (%pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (%pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))
  #-(or sbcl ccl clisp)
  (error "%file-exists-p not implemented."))

(defun %pathname-as-file (pathname)
  (if (%directory-p pathname)
      (let* ((directory (cl:pathname-directory pathname))
	     (name-and-type (cl:pathname (first (last directory)))))
	(cl:make-pathname :directory (butlast directory)
			  :name (cl:pathname-name name-and-type)
			  :type (cl:pathname-type name-and-type)
			  :defaults pathname))
      pathname))

(defun %list-directory (path)
  (let ((wildcard (%directory-wildcard path)))
    #+sbcl
    (directory wildcard)
    #+ccl
    (directory wildcard :directories t)
    #+clisp
    (nconc (directory wildcard)
	   (directory (%subdirectories-wildcard wildcard)))
    #-(or sbcl ccl clisp)
    (error "%list-directory not implemented.")))

;;
;; Conversion between pathnames and fs structures.
;;

(defclass cl-path-root (path-root) 
  ((host :initarg :host :initform nil :accessor path-root-host)
   (device :initarg :device :initform nil :accessor path-root-device)))

(defmethod initialize-instance :after ((obj cl-path-root) &key default)
  (when default
    (setf (path-root-host obj) (path-root-host default))
    (setf (path-root-device obj) (path-root-device default))))

(defclass cl-filename (filename)
  ((version :initarg :version :initform nil :accessor filename-version)))

(defmethod fs-make-filename ((fs (eql 'common-filesystem)))
  (make-instance 'cl-filename))

(defmethod initialize-instance :after ((obj cl-filename) &key default)
  (when default
    (setf (filename-version obj) (filename-version default))))

(defun path-version (path)
  (aif (path-filename path)
       (filename-version it)))

(defun path-host (path)
  (aif (path-root path)
       (path-root-host it)))

(defun path-device (path)
  (aif (path-root path)
       (path-root-device it)))

(defun to-pathname (path)
  (cl:make-pathname :name (path-name path)
		    :type (path-type path)
		    :version (path-version path)
		    :host (path-host path)
		    :device (path-device path)
		    :directory (cons (if (path-relative-p path) :relative :absolute)
				     (path-directory path))))

(defun from-pathname (pathname)
  (flet ((from-cl (value)
	   (if (eq value :wild) "*" value)))
    (make-path :root (make-instance 'cl-path-root
				    :host (from-cl (cl:pathname-host pathname))
				    :device (from-cl (cl:pathname-device pathname)))
	       :relative-p (not (eq (first (cl:pathname-directory pathname)) :absolute))
	       :directory (mapcar #'from-cl (rest (cl:pathname-directory pathname)))
	       :filename (if (not (%directory-p pathname))
			     (make-instance 'cl-filename
					    :name (from-cl (cl:pathname-name pathname))
					    :type (from-cl (cl:pathname-type pathname))
					    :version (from-cl (cl:pathname-version pathname)))))))


;;
;; Implementation for fs-interface for common lisp pathnames.
;;

(defmethod fs-path-from-string ((fs (eql 'common-filesystem)) string)
  (from-pathname (cl:pathname string)))

(defmethod fs-path-to-string ((fs (eql 'common-filesystem)) path)
  (namestring (to-pathname path)))

(defmethod fs-file-exists-p ((fs (eql 'common-filesystem)) file)
  (let ((found-path (%file-exists-p (to-pathname file))))
    (if (not found-path)
	nil
	(not (%directory-p found-path)))))

(defmethod fs-directory-exists-p ((fs (eql 'common-filesystem)) dir)
  (let ((found-path (%file-exists-p (to-pathname dir))))
    (if (not found-path)
        nil 
	(%directory-p found-path))))

(defmethod fs-list-directory ((fs (eql 'common-filesystem)) dir)
  (mapcar #'from-pathname (%list-directory (to-pathname dir))))

(defmethod fs-current-directory ((fs (eql 'common-filesystem)))
  #+clisp
  (from-pathname (ext:absolute-pathname ""))
  #+sbcl
  (from-pathname (cl:merge-pathnames ""))
  #+(and ccl unix)
  (from-pathname (ccl:current-directory))
  #-(or clisp sbcl (and ccl unix))
  (call-next-method))

(defmethod fs-home-directory ((fs (eql 'common-filesystem)))
  (cl:user-homedir-pathname))

(defmethod fs-delete-file ((fs (eql 'common-filesystem)) file)
  (cl:delete-file (to-pathname file)))

(defmethod fs-delete-directory ((fs (eql 'common-filesystem)) dir)
  (let ((path (to-pathname dir)))
    #+clisp
    (ext:delete-directory path)
    #+sbcl
    (sb-ext:delete-directory path)
    #+ccl
    (ccl:delete-directory path)))

(defmethod fs-make-file ((fs (eql 'common-filesystem)) file)
  (cl:open (to-pathname file) :direction :probe :if-does-not-exist :create))

(defmethod fs-make-directory ((fs (eql 'common-filesystem)) dir)
  (ensure-directories-exist (to-pathname dir)))

(defmethod fs-open-file ((fs (eql 'common-filesystem)) path 
			 &key (direction nil direction-p)
			 (element-type nil element-type-p)
			 (if-exists nil if-exists-p)
			 (if-does-not-exist nil if-does-not-exist-p))
  (apply #'cl:open (to-pathname path)
	 (append (if direction-p `(:direction ,direction))
		 (if element-type-p `(:element-type ,element-type))
		 (if if-exists-p `(:if-exists ,if-exists))
		 (if if-does-not-exist-p `(:if-does-not-exist ,if-does-not-exist)))))

(defmethod fs-close-stream ((fs (eql 'common-filesystem)) stream)
  (close stream))

(defmethod fs-as-file-path ((fs (eql 'common-filesystem)) directory-path)
  (from-pathname (%pathname-as-file (to-pathname directory-path))))

(defmethod fs-as-directory-path ((fs (eql 'common-filesystem)) file-path)
  (from-pathname (%pathname-as-directory (to-pathname file-path))))

(defmethod fs-file-length ((fs (eql 'common-filesystem)) path &optional (element-type 'unsigned-byte))
  (with-open-file (stream (to-pathname path) :element-type element-type)
    (cl:file-length stream)))
