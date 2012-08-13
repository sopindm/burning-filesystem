(in-package :burning-filesystem)

(defmacro def-vfs-method (name (fs &rest args) &body body)
  `(defmethod ,name ((,fs virtual-filesystem) ,@args)
     ,@body))

(defstruct (virtual-filesystem (:constructor %make-virtual-filesystem) (:conc-name vfs-))
  root
  current-path)

(defmethod print-object ((object virtual-filesystem) stream)
  (format stream "#S(VIRTUAL-FILESYSTEM :ROOT ~a :CURRENT-PATH ~a)" (vfs-root object) (vfs-current-path object)))

(defun vfs-current (fs)
  (let ((path (vfs-current-path fs)))
    (vfs-find-directory fs (path-relative-p path) (path-directory path))))

(defstruct (vfs-directory (:conc-name vfsd-))
  parent
  directories
  files)

(defmethod print-object ((object vfs-directory) stream)
  (format stream "#S(VFS-DIRECTORY :DIRECTORIES ~a :FILES ~a)" (vfsd-directories object) (vfsd-files object)))

(defstruct (vfs-file (:conc-name vfsf-))
  (value (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
  (readers 0)
  (write-lock-p nil))

(defun make-virtual-filesystem ()
  (let ((home-dir (make-vfs-directory))
	(work-dir (make-vfs-directory)))
    (let ((root (make-vfs-directory :directories (list (cons "home" home-dir) (cons "work" work-dir)))))
      (setf (vfsd-parent home-dir) root)
      (setf (vfsd-parent work-dir) root)
      (let ((fs (%make-virtual-filesystem :root root)))
	(setf (vfs-current-path fs) (fs-path-from-string fs "/work/"))
	fs))))

(defun divide (string char &key (include-middle nil) (from-end nil))
  (let ((pos (position char string :from-end from-end)))
    (if pos
	(list (subseq string 0 pos) (subseq string (if include-middle pos (1+ pos))))
	(list string))))

(defun path-to-list (string)
  (let ((division (divide string #\/)))
    (if (rest division)
	(cons (first division) (path-to-list (second division)))
	division)))

(defun parse-directory (list)
  (let ((path (make-path)))
    (labels 
	((?string (string)
	   (if (and string (not (equal string "")))
	       (list string)
	       nil))
	 (parse-path (string)
	   (setf (path-relative-p path) (not (equal string "")))
	   (setf (path-directory path) (?string string))))
      (parse-path (first list))
      (setf (path-root path) (make-instance 'path-root))
      (setf (path-directory path) (append (path-directory path) (rest list)))
      path)))

(defun parse-file (string)
  (labels 
      ((parse-type (string)
	 (let ((args (divide string #\. :from-end t)))
	   (if (not (eq (position #\. string :from-end t) 0))
	       (make-instance 'filename :name (first args) :type (second args))
	       (make-instance 'filename :name (string+ "." (second args)))))))
    (parse-type string)))

(def-vfs-method fs-path-from-string (fs string)
  (let ((string-list (path-to-list string)))
    (let ((path (parse-directory (butlast string-list)))
	  (filename (if (equal (first (last string-list)) "") 
			nil 
			(parse-file (first (last string-list))))))
      (when filename
	(setf (path-filename path) filename))
      path)))

(defun filestring (path)
  (with-output-to-string (stream)
    (awhen (path-name path)
      (format stream "~a" it))
    (awhen (path-type path)
      (format stream ".~a" it))))

(def-vfs-method fs-path-to-string (fs path)
  (with-output-to-string (stream)
    (unless (path-relative-p path)
      (format stream "/"))
    (format stream "~{~a/~}" (path-directory path))
    (princ (filestring path) stream)))

(def-vfs-method fs-as-file-path (fs path)
  (let ((file (parse-file (first (last (path-directory path))))))
    (copy-path path
	       :new-directory (butlast (path-directory path))
	       :new-name (filename-name file)
	       :new-type (filename-type file))))

(def-vfs-method fs-as-directory-path (fs path)
  (copy-path path 
	     :new-directory (append (path-directory path) (list (filestring path)))
	     :new-filename nil))

(defun vfs-find-directory (fs relative-p path)
  (labels 
      ((do-find-subdirectory (dir name)
	 (cond 
	   ((string= name ".") dir)
	   ((string= name "..") (if (vfsd-parent dir) (vfsd-parent dir) dir))
	   (t (rest (assoc name (vfsd-directories dir) :test #'equal)))))
       (do-find-directory (dir path)
	 (cond 
	   ((null dir) nil)
	   ((null path) dir)
	   ((null (rest path)) (do-find-subdirectory dir (first path)))
	   (t (do-find-directory (do-find-subdirectory dir (first path)) (rest path))))))
    (if relative-p 
	(do-find-directory (vfs-current fs) path)
	(do-find-directory (vfs-root fs) path))))

(defun vfs-find-file (fs path)
  (let ((directory (vfs-find-directory fs (path-relative-p path) (path-directory path))))
    (rest (assoc (list (path-name path) (path-type path))
		 (vfsd-files directory)
		 :test #'equal))))

(def-vfs-method fs-list-directory (fs path)
  (flet ((directory-name (dir)
	   (first (last (path-directory dir))))
	 (subdir (entity)
	   (let ((subdir (copy-path path)))
	     (setf (path-directory subdir) (append (path-directory subdir) (list (first entity))))
	     subdir))
	 (subfile (entity)
	   (destructuring-bind (name type) (first entity)
	     (copy-path path
			:new-name name
			:new-type type))))
    (let ((directory (vfs-find-directory fs (path-relative-p path) (path-directory path))))
      (nconc (sort (mapcar #'subdir (vfsd-directories directory)) #'string< :key #'directory-name)
	     (sort (mapcar #'subfile (vfsd-files directory)) #'string<  :key #'filestring)))))

(def-vfs-method fs-make-directory (fs path)
  (let ((parent (vfs-find-directory fs (path-relative-p path) (butlast (path-directory path)))))
    (push (cons (first (last (path-directory path))) (make-vfs-directory :parent parent))
	  (vfsd-directories parent))))

(defun find-file-directory (fs path)
  (vfs-find-directory fs (path-relative-p path) (path-directory path)))

(def-vfs-method fs-make-file (fs path)
  (let ((parent (find-file-directory fs path)))
    (push (cons (list (path-name path) (path-type path)) (make-vfs-file))
	  (vfsd-files parent))))

(def-vfs-method fs-current-directory (fs)
  (vfs-current-path fs))

(def-vfs-method fs-home-directory (fs)
  (fs-path-from-string fs "/home/"))

(def-vfs-method fs-delete-directory (fs path)
  (let ((parent (vfs-find-directory fs (path-relative-p path) (butlast (path-directory path)))))
    (setf (vfsd-directories parent) 
	  (remove (first (last (path-directory path))) (vfsd-directories parent) :test #'equal :key #'first))))

(def-vfs-method fs-delete-file (fs path)
  (let ((file (vfs-find-file fs path))
	(parent (find-file-directory fs path)))
    (when (or (vfsf-write-lock-p file) (> (vfsf-readers file) 0))
      (error 'file-lock-error :path path))
    (setf (vfsd-files parent)
	  (remove (list (path-name path) (path-type path)) (vfsd-files parent) 
		  :test #'equal :key #'first))))

(def-vfs-method fs-file-exists-p (fs path)
  (vfs-find-file fs path))

(def-vfs-method fs-directory-exists-p (fs path)
  (vfs-find-directory fs (path-relative-p path) (path-directory path)))
    
(def-vfs-method fs-file-replace (fs path action)
  (if (member action '(:new-version :rename))
      (let* ((directory (find-file-directory fs path))
	     (filename (first (assoc (list (path-name path) (path-type path))
				     (vfsd-files directory) :test #'equal))))
	(setf (first filename) (format nil "~a~:[~;~:*.~a~]" (first filename) (second filename)))
	(setf (second filename) "bak")
	(fs-make-file fs path))
      (call-next-method)))

(defun vfs-cd (path)
  (unless (path-exists-p path)
    (error "Cannot cd to non-existing path ~a." path))
  (let ((path (as-absolute-path path)))
    (setf (vfs-current-path (path-filesystem path)) path)))


