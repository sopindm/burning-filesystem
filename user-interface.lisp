(in-package #:burning-filesystem)

(define-condition wrong-filename-error (error) 
  ((string :initarg :string :reader wrong-filename-error-string)))

(defun make-filename (filesystem &key name type)
  (let ((filename (fs-make-filename filesystem)))
    (awhen name (setf (filename-name filename) it))
    (awhen type (setf (filename-type filename) it))
    filename))

;;
;; Path/string conversion
;;

(defvar *default-filesystem* 'common-filesystem)

(defun path-from-string (filename &key (type nil) (fs *default-filesystem*))
  (let ((path (fs-path-from-string fs filename)))
    (flet ((check-path (path)
	     (cond 
	       ((and (eq type :file) (directory-path-p path)) (error 'wrong-filename-error :string filename))
	       ((and (eq type :directory) (file-path-p path)) (fs-as-directory-path fs path))
	       (t path))))
      (setf (path-filesystem path) fs)
      (check-path path))))

(defun path-to-string (path)
  (fs-path-to-string (path-filesystem path) path))

;;
;; Path utitlity functions
;;

(defmacro ensure-path (path)
  `(when (stringp ,path) (setf ,path (path-from-string ,path))))

(defun copy-path (path &key
		  (new-filesystem nil filesystem-p)
		  (new-relative-p nil relative-p)
		  (new-directory nil directory-p)
		  (new-filename nil filename-p)
		  (new-name nil name-p)
		  (new-type nil type-p))
  (ensure-path path)
  (let ((new-path (%copy-path path)))
    (when filesystem-p (setf (path-filesystem new-path) new-filesystem))
    (when relative-p (setf (path-relative-p new-path) new-relative-p))
    (when directory-p (setf (path-directory new-path) new-directory))
    (when filename-p (setf (path-filename new-path) new-filename))
    (when name-p (setf (path-name new-path) new-name))
    (when type-p (setf (path-type new-path) new-type))
    new-path))

(defun path-filesystem (path)
  (ensure-path path)
  (path-root-filesystem (path-root path)))

(defun (setf path-filesystem) (value path)
  (setf (path-root-filesystem (path-root path)) value))

(defun path-name (path)
  (ensure-path path)
  (aif (path-filename path)
       (filename-name it)))

(defun (setf path-name) (value path)
  (aif (path-filename path)
       (setf (filename-name it) value)
       (setf (path-filename path) (make-filename (path-filesystem path) :name value))))

(defun path-type (path)
  (ensure-path path)
  (aif (path-filename path)
       (filename-type it)))

(defun (setf path-type) (value path)
  (aif (path-filename path)
       (setf (filename-type it) value )
       (setf (path-filename path) (make-filename (path-filesystem path) :type value))))

(defun parent-directory (path)
  (ensure-path path)
  (let ((parent (copy-path path :new-filename nil)))
    (if (path-directory parent)
	(progn (setf (path-directory parent) (butlast (path-directory path)))
	       parent)
	nil)))

(defun file-path-p (path)
  (ensure-path path)
  (not (null (path-filename path))))

(defun directory-path-p (path)
  (ensure-path path)
  (null (path-filename path)))

(defun directory-path (path)
  (ensure-path path)
  (if (directory-path-p path)
      path
      (copy-path path :new-filename nil)))

(defun parent-path (path)
  (ensure-path path)
  (if (file-path-p path)
      (directory-path path)
      (parent-directory path)))

(defun root-path (path)
  (ensure-path path)
  (copy-path path :new-directory () :new-filename nil))

(define-condition wrong-path-error (error)
  ((path :initarg :path :reader wrong-path-error-path)))

(defun path-as-file (path)
  (ensure-path path)
  (if (file-path-p path)
      path
      (progn 
	(unless (path-directory path)
	  (error 'wrong-path-error :path path))
	(copy-path (fs-as-file-path (path-filesystem path) path) :new-filesystem (path-filesystem path)))))

(defun path-as-directory (path)
  (ensure-path path)
  (if (directory-path-p path)
      path
      (copy-path (fs-as-directory-path (path-filesystem path) path)
		 :new-filesystem (path-filesystem path))))

(defun path+ (&rest paths)
  (labels ((directory+ (dir1 dir2)
	     (if (absolute-path-p dir2)
		 (copy-path dir1 :new-relative-p nil
			    :new-directory (path-directory dir2))
		 (copy-path dir1 :new-directory (append (path-directory dir1) (path-directory dir2)))))
	   (file+ (path1 path2)
	     (ensure-path path1)
	     (ensure-path path2)
	     (if (file-path-p path1)
		 path2
		 (copy-path (directory+ path1 path2) :new-filename (path-filename path2)))))
    (reduce #'file+ paths)))

(defun path- (path1 path2)
  (ensure-path path1)
  (ensure-path path2)
  (labels ((path-diff (path1 path2)
	     (cond 
	       ((null path2) (values path1 t))
	       ((null path1) (values nil nil))
	       ((equal (first path1) (first path2)) (path-diff (rest path1) (rest path2)))
	       (t (values nil nil)))))
    (if (path= (root-path path1) (root-path path2))
	(multiple-value-bind (diff diff-p) (path-diff (path-directory path1) (path-directory path2))
	  (if diff-p
	      (copy-path path1
			 :new-directory diff
			 :new-relative-p t))))))

;;
;; Checking paths
;;

(defun path-exists-p (path)
  (ensure-path path)
  (let ((fs (path-filesystem path)))
    (labels ((directory-exists-p (path)
	       (cond
		 ((null (path-directory path)) t)
		 (t (and (directory-exists-p (parent-directory path)) 
			 (fs-directory-exists-p fs path))))))
      (and (directory-exists-p (directory-path path))
	   (or (directory-path-p path) (fs-file-exists-p fs path))))))

(defun correct-path-p (path)
  (ensure-path path)
  (cond
    ((path-exists-p path) t)
    ((path-exists-p (path-as-file path)) nil)
    ((path-exists-p (path-as-directory path)) nil)
    (t (path-exists-p (parent-path path)))))

(defun relative-path-p (path)
  (ensure-path path)
  (path-relative-p path))

(defun absolute-path-p (path)
  (ensure-path path)
  (not (path-relative-p path)))

(defun as-absolute-path (path)
  (ensure-path path)
  (labels ((%absolute-path (path dir)
	     (cond
	       ((string= dir ".") path)
	       ((string= dir "..") (butlast path))
	       (t (append path (list dir)))))
	   (%as-absolute-directory (path) 
	     (let ((parent (parent-path path)))
	       (if parent 
		   (copy-path path 
			      :new-directory (%absolute-path (path-directory (as-absolute-path parent))
							     (first (last (path-directory path)))))
		   path)))
	   (%as-absolute-path (path)
	     (if (file-path-p path)
		 (copy-path (as-absolute-path (directory-path path)) :new-filename (path-filename path))
		 (%as-absolute-directory path))))
    (if (relative-path-p path)
	(as-absolute-path (path+ (current-directory (path-filesystem path)) path))
	(%as-absolute-path path))))
	   
(defun as-relative-path (path base)
  (ensure-path path)
  (ensure-path base)
  (labels ((way-up (list)
	     (mapcar #'(lambda (x) (declare (ignore x)) "..") 
		     list))
	   (find-way (list1 list2)
	     (cond 
	       ((null list1) (way-up list2))
	       ((null list2) list1)
	       ((equal (first list1) (first list2)) (find-way (rest list1) (rest list2)))
	       (t (append (way-up list2) list1)))))
    (when (relative-path-p path) (setf path (as-absolute-path path)))
    (when (relative-path-p base) (setf base (as-absolute-path base)))
    (copy-path path :new-relative-p t :new-directory (find-way (path-directory path) (path-directory base)))))

;;
;; Making and deleting filesystem objects
;;

(defun ensure-directory-exists (path create-if-not)
  (unless (path-exists-p path)
    (if create-if-not
	(make-directory path :recursive t)
	(error 'directory-does-not-exist-error :path path)))
  t)

(defun make-file (path &key recursive)
  (ensure-path path)
  (when (directory-path-p path) (error 'wrong-path-error :path path))
  (ensure-directory-exists (parent-path path) recursive)
  (when (path-exists-p path) (error 'file-already-exists-error :path path))
  (unless (correct-path-p path) (error 'no-correct-path-error :path path))
  (fs-make-file (path-filesystem path) path))

(defun make-directory (path &key recursive)
  (ensure-path path)
  (when (file-path-p path) (error 'wrong-path-error :path path))
  (ensure-directory-exists (parent-path path) recursive)
  (when (path-exists-p path) (error 'directory-already-exists-error :path path))
  (unless (correct-path-p path) (error 'no-correct-path-error :path path))
  (fs-make-directory (path-filesystem path) path))

(defun remove-file (path)
  (ensure-path path)
  (when (directory-path-p path) (error 'wrong-path-error :path path))
  (unless (path-exists-p path)
    (error 'file-does-not-exist-error :path path))
  (fs-delete-file (path-filesystem path) path))

(defun remove-directory (path &optional (recursive-p nil))
  (ensure-path path)
  (when (file-path-p path) (error 'wrong-path-error :path path))
  (flet ((remove-path (path)
	   (if (file-path-p path)
	       (remove-file path)
	       (remove-directory path t))))
    (unless (path-exists-p path)
      (error 'directory-does-not-exist-error :path path))
    (when (list-directory path)
      (if recursive-p 
	  (mapc #'remove-path (list-directory path))
	  (error 'directory-not-empty-error :path path)))
    (fs-delete-directory (path-filesystem path) path)))

;;
;; Working with files tree
;;

(defun list-directory (path &optional (recursive-p nil))
  (ensure-path path)
  (unless (directory-path-p path)
    (error 'wrong-path-error :path path))
  (unless (path-exists-p path)
    (error 'directory-does-not-exist-error :path path))
  (labels ((do-list-directory (path)
	     (mapcar #'(lambda (x) (copy-path x :new-filesystem (path-filesystem path)))
		       (fs-list-directory (path-filesystem path) path)))
	   (filter-files (list)
	     (remove-if-not #'file-path-p list))
	   (list-subdirectories (list)
	     (apply #'append (mapcar #'(lambda (dir) (list-directory dir t)) 
				     (remove-if-not #'directory-path-p list)))))
    (if recursive-p 
	(let ((dir-list (do-list-directory path)))
	  (append (filter-files dir-list) (list-subdirectories dir-list)))
	(do-list-directory path))))

(defun match-pattern (pattern string &optional (p-index 0) (s-index 0))
  (let ((pattern-empty-p (= p-index (length pattern)))
	(string-empty-p (= s-index (length string))))
    (cond
      ((and pattern-empty-p string-empty-p) t)
      (pattern-empty-p nil)
      (string-empty-p (and (char= (char pattern p-index) #\*) (match-pattern pattern string (1+ p-index) s-index)))
      ((char= (char pattern p-index) #\*) (or (match-pattern pattern string (1+ p-index) (1+ s-index))
					      (match-pattern pattern string p-index (1+ s-index))))
      ((char= (char pattern p-index) #\?) (match-pattern pattern string (1+ p-index) (1+ s-index)))
      (t (and (char= (char pattern p-index) (char string s-index)) 
	      (match-pattern pattern string (1+ p-index) (1+ s-index)))))))

(defun %resolve-file-path (path)
  (labels ((filestring (path)
	     (format nil "~a.~a" (path-name path) (path-type path)))
	   (do-resolve (dir path)
	     (let ((files (remove-if-not #'file-path-p (list-directory dir))))
	       (remove-if-not #'(lambda (file) (match-pattern (filestring path) (filestring file))) files))))
    (apply #'append (mapcar #'(lambda (dir) (do-resolve dir path)) 
			    (resolve-path (parent-path path))))))

(defun %resolve-directory-path (path)
  (labels ((name (path)
	     (first (last (path-directory path))))
	   (do-resolve (dir path)
	     (let ((dirs (remove-if-not #'directory-path-p (list-directory dir))))
	       (remove-if-not #'(lambda (dir) (match-pattern (name path) (name dir))) dirs))))
    (let ((parent (parent-path path)))
      (if parent
	  (apply #'append (mapcar #'(lambda (dir) (do-resolve dir path))
				  (resolve-path (parent-path path))))
	  (if (path-exists-p path) (list path) nil)))))

(defun resolve-path (path)
  (ensure-path path)
  (if (file-path-p path)
      (%resolve-file-path (as-absolute-path path))
      (%resolve-directory-path (as-absolute-path path))))

;;
;; Standart directories
;;

(defun home-directory (&optional (fs *default-filesystem*))
  (copy-path (fs-home-directory fs) :new-filesystem fs))

(defun current-directory (&optional (fs *default-filesystem*))
  (copy-path (fs-current-directory fs) :new-filesystem fs))

;;
;; Working with streams
;;

(defun open-file (path &key 
		  (direction nil direction-p) 
		  (element-type nil element-p) 
		  (if-exists nil if-exists-p)
		  (if-does-not-exist nil if-does-not-exist-p))
  (ensure-path path)
  (when (directory-path-p path) (error 'wrong-path-error :path path))
  (if (not (correct-path-p path))
      (if (eq direction :probe) nil (error 'no-correct-path-error :path path))
      (apply #'fs-open-file (path-filesystem path) path 
	     (append (if direction-p `(:direction ,direction))
		     (if element-p `(:element-type ,element-type))
		     (if if-exists-p `(:if-exists ,if-exists))
		     (if if-does-not-exist-p `(:if-does-not-exist ,if-does-not-exist))))))

(defun file-length (path &optional (element-type nil element-type-p))
  (ensure-path path)
  (when (directory-path-p path) (error 'wrong-path-error :path path))
  (unless (path-exists-p path) (error 'file-does-not-exist-error :path path))
  (apply #'fs-file-length (path-filesystem path) path (if element-type-p (list element-type))))

(defmacro with-open-file ((stream path &key 
				  (direction nil direction-p) 
				  (element-type nil element-p)
				  (if-exists nil if-exists-p)
				  (if-does-not-exist nil if-does-not-exist-p)) 
			  &body body)
  (let ((path-sym (gensym)))
    `(let ((,path-sym ,path))
       (let ((,stream (open-file ,path-sym
				 ,@(if direction-p `(:direction ,direction))
				 ,@(if element-p `(:element-type ,element-type))
				 ,@(if if-exists-p `(:if-exists ,if-exists))
				 ,@(if if-does-not-exist-p `(:if-does-not-exist ,if-does-not-exist)))))
	 (unwind-protect (progn ,@body)
	   (close ,stream))))))

(defun read-file (path)
  (ensure-path path)
  (with-open-file (stream path)
    (let ((string (make-string (file-length path))))
      (read-sequence string stream)
      string)))

(defun write-file (path string)
  (ensure-path path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write-sequence string stream)))

(defun copy-file (source dest)
  (ensure-path source)
  (ensure-path dest)
  (unless (path-exists-p source)
    (error 'file-does-not-exist-error :path source))
  (when (path-exists-p dest)
    (error 'file-already-exists-error :path dest))
  (unless (correct-path-p dest)
    (error 'no-correct-path-error :path dest))
  (unless (file-path-p source) (error 'wrong-path-error :path source))
  (unless (file-path-p dest) (error 'wrong-path-error :path dest))
  (if (filesystem= (path-filesystem source) (path-filesystem dest))
      (fs-copy-file (path-filesystem source) source dest)
      (%copy-file source dest)))

(defun copy-directory (source dest)
  (ensure-path source)
  (ensure-path dest)
  (unless (path-exists-p source) (error 'directory-does-not-exist-error :path source))
  (when (path-exists-p dest) (error 'directory-already-exists-error :path dest))
  (unless (correct-path-p dest) (error 'no-correct-path-error :path dest))
  (flet ((do-copy ()
	   (make-directory dest)
	   (let ((entities (list-directory source)))
	     (mapc #'(lambda (x) (copy-file x (path+ dest (path- x source)))) 
		   (remove-if-not #'file-path-p entities))
	     (mapc #'(lambda (x) (copy-directory x (path+ dest (path- x source))))
		   (remove-if-not #'directory-path-p entities)))))
    (if (filesystem= (path-filesystem source) (path-filesystem dest))
	(fs-copy-directory (path-filesystem source) source dest)
	(do-copy))))

(defun move-file (source dest)
  (ensure-path source)
  (ensure-path dest)
  (unless (file-path-p source) (error 'wrong-path-error :path source))
  (unless (file-path-p dest) (error 'wrong-path-error :path dest))
  (when (path-exists-p dest) (error 'file-already-exists-error :path dest))
  (unless (correct-path-p dest) (error 'no-correct-path-error :path dest))
  (if (filesystem= (path-filesystem source) (path-filesystem dest))
      (fs-move-file (path-filesystem source) source dest)
      (progn (copy-file source dest) (remove-file source))))

(defun move-directory (source dest)
  (ensure-path source)
  (ensure-path dest)
  (unless (directory-path-p source) (error 'wrong-path-error :path source))
  (unless (directory-path-p dest) (error 'wrong-path-error :path dest))
  (unless (path-exists-p source) (error 'directory-does-not-exist-error :path source))
  (when (path-exists-p dest) (error 'directory-already-exists-error :path dest))
  (unless (correct-path-p dest) (error 'no-correct-path-error :path dest))
  (if (filesystem= (path-filesystem source) (path-filesystem dest))
      (fs-move-directory (path-filesystem source) source dest)
      (progn (copy-directory source dest) (remove-directory source t))))
		
