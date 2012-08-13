(in-package :burning-filesystem-test)

(in-case vfs-test)

(defun make-dp (relative-p path)
  (burning-filesystem::make-path :root (make-instance 'path-root :filesystem *default-filesystem*)
				 :relative-p relative-p
				 :directory path
				 :filename nil))

(defun make-fp (relative-p path name type)
  (let ((path (make-dp relative-p path)))
    (setf (path-filename path) (make-instance 'filename 
					      :name name
					      :type type))
    path))

(defmacro test-vfs (&body body)
  `(let* ((fs (make-virtual-filesystem))
	  (*default-filesystem* fs))
     (declare (ignorable fs))
     ,@body))

(defmacro def-vfs-test (name &body body)
  `(deftest ,name 
     (test-vfs ,@body)))

(define-equality-check path=)

(def-vfs-test basic-string-to-path
  (?path= (path-from-string "bla")
	  (make-fp t () "bla" nil))
  (?path= (path-from-string "/bla-bla")
	   (make-fp nil () "bla-bla" nil))
  (?path= (path-from-string "/bla/bla-bla/bla-bla-bla")
	   (make-fp nil '("bla" "bla-bla") "bla-bla-bla" nil))
  (?path= (path-from-string "bla/bla-bla/bla-bla-bla")
	   (make-fp t '("bla" "bla-bla") "bla-bla-bla" nil))
  (?path= (path-from-string "bla/bla-bla/bla-bla-bla/")
	   (make-dp t '("bla" "bla-bla" "bla-bla-bla")))
  (?path= (path-from-string "")
	  (make-dp t ()))
  (?path= (path-from-string "/")
	  (make-dp nil ())))

(def-vfs-test typed-string-to-path
  (?path= (path-from-string "bla.bla-bla")
	   (make-fp t () "bla" "bla-bla"))
  (?path= (path-from-string ".bla-bla")
	   (make-fp t () ".bla-bla" nil))
  (?path= (path-from-string "bla.bla.bla")
	   (make-fp t nil "bla.bla" "bla")))

(def-vfs-test string-from-path
  (let ((path (make-fp nil '("dir1" "dir2") "name1.name2" "name3")))
    (?equal (path-to-string path) "/dir1/dir2/name1.name2.name3"))
  (let ((path (make-fp t '("dir1" "dir2") "name1" "name2")))
    (!equal (path-to-string path) "dir1/dir2/name1.name2"))
  (let ((path (make-dp nil '("dir"))))
    (!equal (path-to-string path) "/dir/")))

(def-vfs-test default-directory-listing
  (mapcheck ?path= 
	    (fs-list-directory fs (path-from-string "/")) 
	    (list (path-from-string "/home/")
		  (path-from-string "/work/")))
  (?null (fs-list-directory fs (path-from-string "/home/")))
  (?null (fs-list-directory fs (path-from-string "/work/")))
  (?null (fs-list-directory fs (path-from-string ""))))

(deftest directory-making
  (test-vfs
    (fs-make-directory fs (path-from-string "/tmp/"))
    (mapcheck ?path= (fs-list-directory fs (path-from-string "/"))
	      (list (path-from-string "/home/")
		    (path-from-string "/tmp/")
		    (path-from-string "/work/"))))
  (test-vfs
    (fs-make-directory fs (path-from-string "bla/"))
    (mapcheck ?path= (fs-list-directory fs (path-from-string ""))
	      (list (path-from-string "bla/")))
    (mapcheck ?path= (fs-list-directory fs (path-from-string "/work/"))
	      (list (path-from-string "/work/bla/")))))

(def-vfs-test home-and-current-directory
  (?path= (fs-home-directory fs) (fs-path-from-string fs "/home/"))
  (?path= (fs-current-directory fs) (fs-path-from-string fs "/work/")))

(deftest creating-files
  (test-vfs
    (fs-make-file fs (path-from-string "/simple"))
    (mapcheck ?path= (fs-list-directory fs (path-from-string "/"))
	      (list (path-from-string "/home/")
		    (path-from-string "/work/")
		    (path-from-string "/simple"))))
  (test-vfs
    (fs-make-file fs (path-from-string "/bcd"))
    (fs-make-file fs (path-from-string "/bce"))
    (fs-make-file fs (path-from-string "/abc"))
    (mapcheck ?path= (fs-list-directory fs (path-from-string "/"))
	      (list (path-from-string "/home/")
		    (path-from-string "/work/")
		    (path-from-string "/abc")
		    (path-from-string "/bcd")
		    (path-from-string "/bce"))))
  (test-vfs
    (fs-make-file fs (path-from-string "/home/file.ext"))
    (mapcheck ?path= (fs-list-directory fs (path-from-string "/home/"))
	      (list (path-from-string "/home/file.ext")))))

(deftest deleting-directories
  (test-vfs
    (fs-delete-directory fs (path-from-string "/home/"))
    (mapcheck ?path= 
	      (fs-list-directory fs (path-from-string "/"))
	      (list (path-from-string "/work/"))))
  (test-vfs
    (fs-make-directory fs (path-from-string "/home/tmp/"))
    (fs-make-directory fs (path-from-string "/home/tmp/tmp2/"))
    (fs-delete-directory fs (path-from-string "/home/tmp/tmp2/"))
    (?null (fs-list-directory fs (path-from-string "/home/tmp/")))
    (fs-delete-directory fs (path-from-string "/home/tmp/"))
    (?null (fs-list-directory fs (path-from-string "/home/")))))

(def-vfs-test deleting-files
  (fs-make-directory fs (path-from-string "tmp/"))
  (fs-make-file fs (path-from-string "tmp/bla.bla"))
  (fs-make-file fs (path-from-string "tmp/bla.bla2"))
  (fs-delete-file fs (path-from-string "tmp/bla.bla"))
  (mapcheck ?path= (fs-list-directory fs (path-from-string "tmp/"))
	    (list (path-from-string "tmp/bla.bla2"))))

(def-vfs-test file-exists-p-test
  (fs-make-file fs (path-from-string "a-file"))
  (fs-make-directory fs (path-from-string "a-directory/"))
  (fs-make-file fs (path-from-string "a-directory/an.another.file"))
  (!t (fs-file-exists-p fs (path-from-string "/work/a-file")))
  (!null (fs-directory-exists-p fs (path-from-string "/work/a-file/")))
  (!null (fs-file-exists-p fs (path-from-string "/work/a-directory")))
  (!t (fs-directory-exists-p fs (path-from-string "/work/a-directory/")))
  (!t (fs-file-exists-p fs (path-from-string ".././work/.././work/./a-directory/an.another.file")))
  (!t (fs-directory-exists-p fs (path-from-string "././../work/./.././work/a-directory/../a-directory/"))))
  
(def-vfs-test path-conversion-test
  (?path= (fs-as-file-path fs (path-from-string "/dir1/dir2/dir3.dir4/"))
	  (make-fp nil '("dir1" "dir2") "dir3" "dir4"))
  (?path= (fs-as-directory-path fs (path-from-string "/dir1/dir2/dir3.dir4"))
	  (make-dp nil '("dir1" "dir2" "dir3.dir4")))
  (?path= (fs-as-directory-path fs (path-from-string ".."))
	  (make-dp t '("..")))
  (?path= (fs-as-file-path fs (path-from-string "../"))
	  (make-fp t () "." "")))

(defun ?file-copied (source dest)
  (let ((text (read-file source)))
    (!t (path-exists-p source))
    (!t (path-exists-p dest))
    (?equal (read-file dest) text)
    (write-file source "other text")
    (?equal (read-file dest) text)
    (write-file dest "some other text")
    (?equal (read-file source) "other text")
    (?equal (read-file dest) "some other text")))

(defun ?file-moved (source dest text)
  (?null (path-exists-p source))
  (?equal (read-file dest) text)
  (write-file dest "other text")
  (?equal (read-file dest) "other text"))

(defun ?directory-copied (source dest)
  (?t (path-exists-p source))
  (?t (path-exists-p dest)))

(defun ?directory-moved (source dest)
  (?null (path-exists-p source))
  (?t (path-exists-p dest)))

(def-vfs-test copying-files-test
  (let ((source (path-from-string "a.file"))
	(dest (path-from-string "other.file")))
    (write-file source "some data")
    (fs-copy-file fs source dest)
    (?file-copied source dest)))

(def-vfs-test copying-big-files-test
  (let ((source (path-from-string "a.file"))
	(dest (path-from-string "other.file"))
	(text (make-string 10000 :initial-element #\a)))
    (write-file source text)
    (fs-copy-file fs source dest)
    (?file-copied source dest)))

(def-vfs-test copying-empty-directories
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (make-directory source)
    (fs-copy-directory fs source dest)
    (?directory-copied source dest)))

(def-vfs-test copying-directories-with-files
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/"))
	(source-file1 (path-from-string "a.dir/file1"))
	(source-file2 (path-from-string "a.dir/other.file")))
    (make-directory source)
    (write-file source-file1 "file1-data")
    (write-file source-file2 "file2-data")
    (fs-copy-directory fs source dest)
    (?directory-copied source dest)
    (?file-copied source-file1 (path-from-string "other.dir/file1"))
    (?file-copied source-file2 (path-from-string "other.dir/other.file"))))

(def-vfs-test copying-directories-with-subdirectories
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/"))
	(source-file1 (path-from-string "a.dir/file1"))
	(source-dir (path-from-string "a.dir/b.dir/"))
	(source-dir2 (path-from-string "a.dir/c.dir/"))
	(source-file2 (path-from-string "a.dir/b.dir/file2")))
    (make-directory source)
    (make-directory source-dir)
    (make-directory source-dir2)
    (write-file source-file1 "file1-data")
    (write-file source-file2 "file2-data")
    (fs-copy-directory fs source dest)
    (?directory-copied source dest)
    (?directory-copied source-dir (path-from-string "other.dir/b.dir/"))
    (?directory-copied source-dir2 (path-from-string "other.dir/c.dir/"))
    (?file-copied source-file1 (path-from-string "other.dir/file1"))
    (?file-copied source-file2 (path-from-string "other.dir/b.dir/file2"))))

(def-vfs-test moving-file
  (let ((source (path-from-string "a.file"))
	(dest (path-from-string "other.file"))
	(text "some data"))
    (write-file source text)
    (fs-move-file fs source dest)
    (?file-moved source dest text)))

(def-vfs-test moving-empty-directory
  (let ((source-path (path-from-string "a.dir/"))
	(dest-path (path-from-string "other.dir/")))
    (make-directory source-path)
    (fs-move-directory fs source-path dest-path)
    (?t (path-exists-p dest-path))
    (?not (path-exists-p source-path))))

(def-vfs-test moving-directory-with-files 
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/"))
	(source-file (path-from-string "a.dir/a.file"))
	(text "text for file1")
	(source-file2 (path-from-string "a.dir/other.file"))
	(text2 "text for file2"))
    (make-directory source)
    (write-file source-file text)
    (write-file source-file2 text2)
    (fs-move-directory fs source dest)
    (?directory-moved source dest)
    (?file-moved source-file (path-from-string "other.dir/a.file") text)
    (?file-moved source-file2 (path-from-string "other.dir/other.file") text2)))

(def-vfs-test moving-directory-with-subdirs 
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/"))
	(source-file (path-from-string "a.dir/a.file"))
	(text "text for file1")
	(source-dir (path-from-string "a.dir/b.dir/"))
	(source-dir2 (path-from-string "a.dir/c.dir/"))
	(source-file2 (path-from-string "a.dir/b.dir/other.file"))
	(text2 "text for file2"))
    (make-directory source)
    (write-file source-file text)
    (make-directory source-dir)
    (make-directory source-dir2)
    (write-file source-file2 text2)
    (fs-move-directory fs source dest)
    (?directory-moved source dest)
    (?file-moved source-file (path-from-string "other.dir/a.file") text)
    (?directory-moved source-dir (path-from-string "other.dir/b.dir/"))
    (?directory-moved source-dir2 (path-from-string "other.dir/c.dir/"))
    (?file-moved source-file2 (path-from-string "other.dir/b.dir/other.file") text2)))
