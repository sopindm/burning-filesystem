(in-package #:burning-filesystem-test)

(in-case ui-test)

(defmacro def-ui-test (name &body body)
  `(deftest ,name
     (let* ((fs (make-virtual-filesystem))
	    (*default-filesystem* fs))
       ,@body)))

(def-ui-test simple-filename-reading
  (?path= (path-from-string "a.file.name")
	  (make-fp t () "a.file" "name")))

(def-ui-test filename-with-directory-reading 
  (?path= (path-from-string "/path/to/directory/file.name")
	  (make-fp nil '("path" "to" "directory") "file" "name")))

(def-ui-test wrong-filename-reading
  (!condition (path-from-string "/path/" :type :file) wrong-filename-error
	      (wrong-filename-error-string "/path/")))

(def-ui-test simple-directory-name-reading
  (?path= (path-from-string "tmp/")
	  (make-dp t '("tmp"))))

(def-ui-test absolute-directory-name-reading
  (?path= (path-from-string "/a.root.directory/")
	  (make-dp nil '("a.root.directory"))))

(def-ui-test complex-relative-directory-name-reading
  (?path= (path-from-string "dir1/dir2/dir3/my.dir/")
	  (make-dp t '("dir1" "dir2" "dir3" "my.dir"))))
	    
(def-ui-test complex-absolute-directory-name-reading
  (?path= (path-from-string "/root/dir1/dir2/the.last.dir/")
	  (make-dp nil '("root" "dir1" "dir2" "the.last.dir"))))

(def-ui-test directory-name-in-file-form-reading
  (?path= (path-from-string "/root/dir1/dir2/dir3" :type :directory)
	  (make-dp nil '("root" "dir1" "dir2" "dir3"))))

(def-ui-test simple-filename-writing
  (!equal (path-to-string (make-fp t () "a.file.name" "an.ext")) "a.file.name.an.ext"))

(def-ui-test complex-filename-writing
  (!equal (path-to-string (make-fp nil '("dir1" "dir2" "dir3") "file" "ext"))
	  "/dir1/dir2/dir3/file.ext"))

(def-ui-test relative-filename-writing
  (!equal (path-to-string (make-fp t '("dir1" "dir2" "dir3") "file" "ext"))
	  "dir1/dir2/dir3/file.ext"))

(def-ui-test relative-directory-name-writting
  (!equal (path-to-string (make-dp t '("dir1" "dir2"))) "dir1/dir2/"))

(def-ui-test absolute-directory-name-writting
  (!equal (path-to-string (make-dp nil '("a.dir" "b.dir"))) "/a.dir/b.dir/"))

(def-ui-test file-path-exists-p-test
  (let ((path1 (path-from-string "a.file"))
	(path1-with-dots (path-from-string ".././././work/a.file"))
	(path1-as-directory (path-from-string "a.file/"))
	(path2 (path-from-string "/work/a.file"))
	(path3 (path-from-string "/work/other.file")))
    (fs-make-file fs path1)
    (?t (path-exists-p path1))
    (?t (path-exists-p path1-with-dots))
    (?t (path-exists-p path2))
    (?not (path-exists-p path1-as-directory))
    (?not (path-exists-p path3))))

(def-ui-test directory-path-exists-p-test
  (let ((path1 (path-from-string "directory/"))
	(absolute-path1 (path-from-string "/work/directory/"))
	(path1-fileform (path-from-string "/work/directory"))
	(path2 (path-from-string "other.directory/")))
    (fs-make-directory fs path1)
    (!t (path-exists-p path1))
    (!t (path-exists-p absolute-path1))
    (!not (path-exists-p path1-fileform))
    (!not (path-exists-p path2))))

(def-ui-test path-exists-p-in-non-existing-directory
  (let ((path1 (path-from-string "dir/file"))
	(path2 (path-from-string "dir/dir/")))
    (!not (path-exists-p path1))
    (!not (path-exists-p path2))))

(def-ui-test path-exists-p-string-argument
  (let ((string1 "some.file.path")
	(string2 "some.directory/"))
    (make-file (path-from-string string1))
    (make-directory (path-from-string string2))
    (?t (path-exists-p string1))
    (?t (path-exists-p string2))))

(def-ui-test make-file-test
  (let ((path (path-from-string "a.file")))
    (make-file path)
    (!t (path-exists-p path))))

(def-ui-test make-file-recursive-test
  (let ((path (path-from-string "/dir1/dir2/an.inner.file")))
    (make-file path :recursive t)
    (!t (path-exists-p path)))
  (let ((path (path-from-string "dir.a/dir.b/dir.c/an.another.inner.file")))
    (make-file path :recursive t)
    (!t (path-exists-p path))))

(def-ui-test make-file-in-non-existing-directory-error
  (let ((dir (path-from-string "dir/"))
	(path (path-from-string "dir/file")))
    (?condition (make-file path) directory-does-not-exist-error
		(filesystem-error-path dir :test path=))))

(def-ui-test make-file-when-directory-exists
  (let ((path (path-from-string "a.file")))
    (make-directory (path-as-directory path))
    (?condition (make-file path)
		no-correct-path-error (filesystem-error-path path :test path=))))

(def-ui-test make-file-with-directory-path
  (let ((path (path-from-string "a.file/")))
    (?condition (make-file path)
		wrong-path-error (wrong-path-error-path path :test path=))))

(def-ui-test make-directory-test
  (let ((path (path-from-string "a.directory/")))
    (make-directory path)
    (!t (path-exists-p path))))

(def-ui-test make-directory-recursive-test
  (let ((path (path-from-string "/dir1/dir2/an.inner.directory/")))
    (make-directory path :recursive t)
    (!t (path-exists-p path)))
  (let ((path (path-from-string "dir.a/dir.b/dir.c/inner.directory/")))
    (make-directory path :recursive t)
    (!t (path-exists-p path))))

(def-ui-test make-directory-in-non-existing-directory
  (let ((parent (path-from-string "outer.dir/"))
	(path (path-from-string "outer.dir/inner.dir/")))
    (!condition (make-directory path) directory-does-not-exist-error
		(filesystem-error-path parent :test path=))))

(def-ui-test making-existing-directory
  (let ((path (path-from-string "a.dir/")))
    (make-directory path)
    (?condition (make-directory path) 
		directory-already-exists-error (filesystem-error-path path :test path=))))

(def-ui-test making-directory-when-file-exists
  (let ((path (path-from-string "a.dir/")))
    (make-file (path-as-file path))
    (?condition (make-directory path)
		no-correct-path-error (filesystem-error-path path :test path=))))

(def-ui-test making-directory-with-file-path
  (let ((path (path-from-string "a.dir")))
    (?condition (make-directory path)
		wrong-path-error (wrong-path-error-path path :test path=))))

(def-ui-test correct-file-path-on-existing-file 
  (let ((path (path-from-string "a.file")))
    (make-file path)
    (!t (correct-path-p path))))

(def-ui-test correct-file-path-on-existing-directory-path
  (let ((path (path-from-string "a.file"))
	(path-as-directory (path-from-string "a.file/")))
    (make-directory path-as-directory)
    (!not (correct-path-p path))))

(def-ui-test correct-directory-path-on-existing-directory
  (let ((path (path-from-string "a.dir/")))
    (make-directory path)
    (!t (correct-path-p path))))

(def-ui-test correct-directory-path-on-existing-file
  (let ((path (path-from-string "a.dir/"))
	(path-as-file (path-from-string "a.dir")))
    (make-file path-as-file)
    (!not (correct-path-p path))))

(def-ui-test correct-path-p-in-existing-directory
  (let ((path (path-from-string "/home/a.file")))
    (!t (correct-path-p path))))

(def-ui-test correct-path-p-in-non-existing-directory
  (let ((path (path-from-string "/dir/file")))
    (!not (correct-path-p path))))

(defmacro path-check (name function result)
  `(?path= (,function (path-from-string ,name))
	   (path-from-string ,result)))

(defmacro ?parent= (name parent)
  `(path-check ,name parent-path ,parent))

(def-ui-test parent-path-for-file
  (?parent= "a.file" "")
  (?parent= "dir1/dir2/file.ext" "dir1/dir2/")
  (?parent= "/dir/file.ext" "/dir/"))

(def-ui-test parent-path-for-directory
  (?parent= "a.dir/" "")
  (?parent= "/a.dir/" "/")
  (?parent= "dir1/dir2/" "dir1/")
  (?parent= "/dir.a/dir.b/" "/dir.a/")
  (?null (parent-path (path-from-string "")))
  (?null (parent-path (path-from-string "/"))))

(defmacro ?as-file= (name file-name)
  `(path-check ,name path-as-file ,file-name))

(def-ui-test file-path-as-file
  (?as-file= "a.file" "a.file")
  (?as-file= "dir/file" "dir/file")
  (?as-file= "/dir/file" "/dir/file"))

(def-ui-test directory-path-as-file
  (?as-file= "dir/" "dir")
  (?as-file= "/dir/" "/dir")
  (let ((path (path-from-string "")))
    (?condition (path-as-file path) wrong-path-error
		(wrong-path-error-path path :test path=)))
  (let ((path (path-from-string "/")))
    (?condition (path-as-file path) wrong-path-error
		(wrong-path-error-path path :test path=))))

(defmacro ?as-directory= (name directory-name)
  `(path-check ,name path-as-directory ,directory-name))

(def-ui-test file-path-as-directory
  (?as-directory= "file" "file/")
  (?as-directory= "/file" "/file/")
  (?as-directory= "dir/file" "dir/file/")
  (?as-directory= "/dir/file" "/dir/file/"))

(def-ui-test directory-path-as-directory
  (?as-directory= "dir/" "dir/")
  (?as-directory= "/dir/" "/dir/")
  (?as-directory= "dir1/dir2/" "dir1/dir2/")
  (?as-directory= "/dir1/dir2/" "/dir1/dir2/"))

(def-ui-test path-type-p-test
  (let ((paths (mapcar #'path-from-string '("file" "/file" "/dir/file" "dir/file"))))
    (?every #'file-path-p paths)
    (?every #'(lambda (x) (not (directory-path-p x))) paths))
  (let ((paths (mapcar #'path-from-string '("dir/" "/dir/" "/dir1/dir2/" "dir1/dir2/"))))
    (!every #'directory-path-p paths)
    (!every #'(lambda (x) (not (file-path-p x))) paths)))

(def-ui-test relative-path-p-test
  (let ((rfp (path-from-string "a.dir/a.file"))
	(rdp (path-from-string "dir.1/dir.2/"))
	(afp (path-from-string "/a.dir/a.file"))
	(adp (path-from-string "/a.dir/b.dir/")))
    (?t (relative-path-p rfp))
    (?t (relative-path-p rdp))
    (?not (relative-path-p afp))
    (?not (relative-path-p adp))
    (?t (absolute-path-p afp))
    (?t (absolute-path-p adp))
    (?not (absolute-path-p rfp))
    (?not (absolute-path-p rdp))))

(def-ui-test relative-path-p-with-strings
  (let ((path1 "a.dir/a.file")
	(path2 "/dir1/other.file"))
    (?t (relative-path-p path1))
    (?null (relative-path-p path2))
    (?t (absolute-path-p path2))
    (?null (absolute-path-p path1))))

(def-ui-test make-directory-path-test 
  (let* ((base-directory (make-dp 'relative-p 'a-path))
	 (directory (copy-path base-directory)))
    (?eq (relative-path-p directory) 'relative-p)
    (?eq (path-directory directory) 'a-path))
  (let* ((default-directory (make-dp 'relative-p 'default-path))
	 (directory (copy-path default-directory :new-directory 'other-path)))
    (?eq (path-directory directory) 'other-path)
    (?eq (relative-path-p directory) 'relative-p)))

(def-ui-test make-file-path-test
  (let ((directory (make-dp 'relative-p 'a-path)))
    (let* ((base-path (make-fp 'relative-p 'a-path 'name 'my-type))
	   (path (copy-path base-path)))
      (?path= (parent-path path) directory)
      (?eq (path-name path) 'name)
      (?eq (path-type path) 'my-type))
    (let* ((default-path (make-fp 'relative-p 'a-path 'default-name 'default-type))
	   (path (copy-path default-path :new-name 'my-name)))
      (?path= (parent-path path) directory)
      (?eq (path-name path) 'my-name)
      (?eq (path-type path) 'default-type))))

(defmacro ?root= (name root-name)
  `(path-check ,name root-path ,root-name))

(def-ui-test root-path-test
  (?root= "a.file" "")
  (?root= "many/long/relative/directories/a.file" "")
  (?root= "/absolute.file" "/")
  (?root= "/many/long/absolute/directories/a.file" "/")
  (?root= "a.relative.directory/" "")
  (?root= "a/long/path/to/relative/directory/" "")
  (?root= "/an.absolute.directory/" "/")
  (?root= "/a/very/long/path/to/absolute/directory/" "/"))

(def-ui-test directory-concatenation
  (let ((r-dir1 (path-from-string "dir1/dir2/"))
	(r-dir2 (path-from-string "dir3/dir4/"))
	(a-dir1 (path-from-string "/a.dir/b.dir/"))
	(a-dir2 (path-from-string "/c.dir/d.dir/")))
    (?path= (path+ r-dir1 r-dir2) (path-from-string "dir1/dir2/dir3/dir4/"))
    (?path= (path+ a-dir1 r-dir1) (path-from-string "/a.dir/b.dir/dir1/dir2/"))
    (?path= (path+ a-dir1 a-dir2) (path-from-string "/c.dir/d.dir/"))
    (?path= (path+ r-dir1 a-dir2) (path-from-string "/c.dir/d.dir/"))))

(def-ui-test directory-file-concatenation
  (let ((r-dir (path-from-string "dir1/dir2/"))
	(a-dir (path-from-string "/dir1/dir2/"))
	(r-file (path-from-string "dir/file"))
	(a-file (path-from-string "/dir/file.ext")))
    (!path= (path+ r-dir r-file) (path-from-string "dir1/dir2/dir/file"))
    (!path= (path+ r-dir a-file) (path-from-string "/dir/file.ext"))
    (!path= (path+ a-dir r-file) (path-from-string "/dir1/dir2/dir/file"))
    (!path= (path+ a-dir a-file) a-file)
    (!path= (path+ a-file a-file) a-file)
    (!path= (path+ a-file r-file) r-file)
    (!path= (path+ r-file a-file) a-file)
    (!path= (path+ r-file r-file) r-file)
    (!path= (path+ a-file r-dir) r-dir)
    (!path= (path+ r-file r-dir) r-dir)
    (!path= (path+ a-dir r-dir r-dir r-file) (path-from-string "/dir1/dir2/dir1/dir2/dir1/dir2/dir/file"))))

(def-ui-test path-delta-test 
  (!path= (path- (path-from-string "/dir1/dir2/dir3/a.file") (path-from-string "/dir1/dir2/"))
	  (path-from-string "dir3/a.file"))
  (?null (path- (path-from-string "/dir1/dir2/dir3/a.file") (path-from-string "dir1/dir2/")))
  (!path= (path- (path-from-string "/dir1/dir2/dir3/") (path-from-string "/dir1/dir2/"))
	  (path-from-string "dir3/"))
  (?null (path- (path-from-string "/dir1/") (path-from-string "/dir2/")))
  (?path= (path- (path-from-string "/dir/") (path-from-string "/dir/"))
	  (path-from-string "")))

(def-ui-test properties-concatenation 
  (let ((directory1 (make-dp nil '("dir1" "dir2")))
	(directory2 (make-dp t '("dir3" "dir4"))))
    (let ((file1 (copy-path directory1 :new-name 'name1 :new-type 'type1))
	  (file2 (copy-path directory2 :new-name 'name2 :new-type 'type2)))
      (!path= (path+ directory1 file2)
	      (copy-path directory1 
			 :new-directory '("dir1" "dir2" "dir3" "dir4") 
			 :new-name 'name2
			 :new-type 'type2))
      (!path= (path+ directory2 file1)
	      (copy-path file1 :new-filesystem (path-filesystem directory2)))
      (!path= (path+ directory1 directory2)
	      (copy-path directory1 :new-directory '("dir1" "dir2" "dir3" "dir4")))
      (!path= (path+ directory2 directory1)
	      (copy-path directory2
			 :new-filesystem (path-filesystem directory1)
			 :new-directory (path-directory directory1) :new-relative-p nil)))))

(def-ui-test delete-file-test
  (flet ((delete-test (name)
	   (let ((path (path-from-string name)))
	     (make-file path)
	     (!t (path-exists-p path))
	     (remove-file path)
	     (!null (path-exists-p path)))))
    (delete-test "file.ext")
    (delete-test "/home/file.ext")
    (make-directory (path-from-string "dir/"))
    (delete-test "dir/file.ext")))

(def-ui-test delete-file-fail-test
  (flet ((fail-test (name)
	   (let ((path (path-from-string name)))
	     (!condition (remove-file path) file-does-not-exist-error
			 (filesystem-error-path path)))))
    (fail-test "file.ext")
    (fail-test "home")
    (fail-test "dir/file.ext")))

(def-ui-test deleting-file-with-directory-path
  (let ((path (path-from-string "a.file/")))
    (make-directory path)
    (?condition (remove-file path) 
		wrong-path-error (wrong-path-error-path path :test path=))))

(def-ui-test delete-directory-test
  (flet ((delete-test (name)
	   (let ((path (path-from-string name)))
	     (make-directory path)
	     (!t (path-exists-p path))
	     (remove-directory path)
	     (!null (path-exists-p path)))))
    (delete-test "a.dir/")
    (make-directory (path-from-string "dir/"))
    (delete-test "dir/other.dir/")
    (delete-test "/work/a.sample.dir/")))
    
(def-ui-test delete-directory-with-files-fail-test
  (let ((path (path-from-string "dir/")))
    (make-directory path)
    (make-file (path-from-string "dir/a.file"))
    (!condition (remove-directory path) directory-not-empty-error
		(filesystem-error-path path))))

(def-ui-test delete-directory-with-subdirectories-fail-test
  (let ((path (path-from-string "dir/")))
    (make-directory path)
    (make-directory (path-from-string "dir/a.subdirectory/"))
    (!condition (remove-directory path) directory-not-empty-error
		(filesystem-error-path path))))

(def-ui-test delete-not-existing-directory-fail
  (let ((path (path-from-string "a.dir/")))
    (!condition (remove-directory path)	directory-does-not-exist-error
		(filesystem-error-path path :test path=))))

(def-ui-test delete-directory-recursive-test
  (let ((path (path-from-string "dir/"))
	(file-path (path-from-string "dir/file"))
	(subdir-path (path-from-string "dir/subdir/"))
	(subdir-file-path (path-from-string "dir/subdir/file")))
    (make-directory path)
    (make-file file-path)
    (make-directory subdir-path)
    (make-file subdir-file-path)
    (remove-directory path t)
    (?null (path-exists-p path))))

(def-ui-test delete-directory-with-file-path
  (let ((path (path-from-string "a.dir")))
    (make-file path)
    (?condition (remove-directory path)
		wrong-path-error (wrong-path-error-path path :test path=))))

(def-ui-test list-empty-directory
  (!null (list-directory (path-from-string "/home/")))
  (!null (list-directory (path-from-string "/work/"))))

(def-ui-test list-directory-with-files
  (let ((path (path-from-string "a.dir/"))
	(file1 (path-from-string "a.dir/file.1"))
	(file2 (path-from-string "a.dir/file.2")))
    (make-directory path)
    (make-file file1)
    (make-file file2)
    (mapcheck ?path= (list-directory path) (list file1 file2))))

(def-ui-test list-directory-with-subdirs
  (let ((path (path-from-string "a.dir/"))
	(file1 (path-from-string "a.dir/file.1"))
	(file2 (path-from-string "a.dir/file.2"))
	(dir1 (path-from-string "a.dir/dir.1/"))
	(dir2 (path-from-string "a.dir/dir.2/")))
    (make-directory path)
    (make-file file1)
    (make-file file2)
    (make-directory dir1)
    (make-directory dir2)
    (mapcheck ?path= (list-directory path) (list dir1 dir2 file1 file2))))

(def-ui-test list-big-directory 
  (let ((path (path-from-string "a.dir/"))
	(file1 (path-from-string "a.dir/a.file"))
	(file2 (path-from-string "a.dir/b.file"))
	(dir1 (path-from-string "a.dir/a.dir/"))
	(dir2 (path-from-string "a.dir/b.dir/"))
	(subfile1 (path-from-string "a.dir/a.dir/sub.file"))
	(subfile2 (path-from-string "a.dir/b.dir/sub.file2")))
    (make-directory path)
    (make-file file1)
    (make-file file2)
    (make-directory dir1)
    (make-directory dir2)
    (make-file subfile1)
    (make-file subfile2)
    (mapcheck ?path= (list-directory path t) (list file1 file2 subfile1 subfile2))))

(def-ui-test list-non-existing-directory
  (let ((path (path-from-string "a.dir/")))
    (!condition (list-directory path) directory-does-not-exist-error
		(filesystem-error-path path))))

(def-ui-test list-directory-with-file-path
  (let ((path (path-from-string "a.dir")))
    (?condition (list-directory path)
		wrong-path-error (wrong-path-error-path path :test path=))))

(def-ui-test resolve-simple-path
  (let ((path (path-from-string "/a.file")))
    (make-file path)
    (mapcheck ?path= (resolve-path path) (list path)))
  (let ((path (path-from-string "/a.dir/")))
    (make-directory path)
    (mapcheck ?path= (resolve-path path) (list path))))

(def-ui-test resolve-long-path
  (let ((path (path-from-string "/a/very/long/path/to/this.file")))
    (make-file path :recursive t)
    (mapcheck ?path= (resolve-path path) (list path))))

(def-ui-test resolve-non-existing-path
  (let ((path (path-from-string "a/non/existing/path/to/a.file")))
    (?null (resolve-path path))))

(def-ui-test resolve-wild-filenames
  (let ((path1 (path-from-string "/a.dir/b.dir/a.file.extbla"))
	(path2 (path-from-string "/a.dir/b.dir/b.file.extmyetx"))
	(wild-path (path-from-string "/a.dir/b.dir/?.file.ext*")))
    (make-file path1 :recursive t)
    (make-file path2)
    (mapcheck ?path= (resolve-path wild-path) (list path1 path2))))

(def-ui-test resolve-wild-pathes
  (let ((path1 (path-from-string "/a.dir/b.dir/c.dir/a.file.name"))
	(path2 (path-from-string "/a.d.r/babr/c.dir/e.blabla.name"))
	(wild-path (path-from-string "/a.d?r/b*r/c.dir/?.*.name")))
    (make-file path1 :recursive t)
    (make-file path2 :recursive t)
    (mapcheck ?path= (resolve-path wild-path) (list path2 path1))))

(def-ui-test resolve-pathes-with-dots 
  (let ((path (path-from-string "../work/")))
    (mapcheck ?path= (resolve-path path) (list (path-from-string "/work/")))))

(defmacro ?as-absolute= (path expected)
  `(path-check ,path as-absolute-path ,expected))

(def-ui-test as-absolute-path-test
  (?as-absolute= "a.dir/other.dir/file.ext" "/work/a.dir/other.dir/file.ext")
  (?as-absolute= "/dir/other.dir/etc" "/dir/other.dir/etc")
  (?as-absolute= "a.dir/../other.dir/./dir/./file.ext" "/work/other.dir/dir/file.ext"))

(defmacro ?as-relative= ((path base) expected)
  `(?equal (path-to-string (as-relative-path (path-from-string ,path) (path-from-string ,base)))
	   ,expected))

(def-ui-test as-relative-path-test
  (?as-relative= ("a.dir/a.file" "") "a.dir/a.file")
  (?as-relative= ("dir1/dir2/dir3/a.file" "dir1/dir2/") "dir3/a.file")
  (?as-relative= ("dir1/dir2/dir3/a.file" "dir1/a.dir/dir2/") "../../dir2/dir3/a.file")
  (?as-relative= ("/dir1/dir2/a.file" "/dir1/other.dir/") "../dir2/a.file")
  (?as-relative= ("dir/a.file" "/dir/") "../work/dir/a.file")
  (?as-relative= ("/dir/a.file" "dir/") "../../dir/a.file")
  (?as-relative= ("dir/file" "other.dir/../dir/") "file"))

(def-ui-test home-directory-test
  (!path= (home-directory fs) (path-from-string "/home/")))

(def-ui-test current-directory-test
  (!path= (current-directory fs) (path-from-string "/work/")))

(def-ui-test opening-no-correct-file-path
  (let ((path (path-from-string "a.dir/a.path")))
    (?condition (open-file path)
		no-correct-path-error (filesystem-error-path path :test path=))))

(def-ui-test open-file-with-directory-path
  (let ((path (path-from-string "a.file/")))
    (?condition (open-file path)
		wrong-path-error (wrong-path-error-path path :test path=))))

;; file length

(def-ui-test file-length-with-default-element-type
  (let ((path (path-from-string "a.file")))
    (with-open-file (stream path :direction :output :element-type 'unsigned-byte)
      (write-sequence '(1 2 3 4 5) stream))
    (?= (file-length path) 5)))

(def-ui-test file-length-with-set-element-type
  (let ((path (path-from-string "a.file")))
    (write-file path "a file")
    (?= (file-length path 'character) 6)))

(def-ui-test file-length-with-unexising-path
  (let ((path (path-from-string "a.file")))
    (?condition (file-length path)
		file-does-not-exist-error
		(filesystem-error-path path :test path=))))

(def-ui-test file-length-with-directory-path
  (let ((path (path-from-string "a.dir/")))
    (make-directory path)
    (?condition (file-length path)
		wrong-path-error (wrong-path-error-path path :test path=))))

(def-ui-test with-file-test
  (let ((path (path-from-string "a.file")))
    (with-open-file (out path :direction :output)
      (write-line "A test string" out))
    (with-open-file (in path :direction :input)
      (!equal (read-line in) "A test string"))))

(def-ui-test read-file-test 
  (let ((path (path-from-string "file" :fs fs))
	(string (lines "a very long line"
			 "and another one")))
    (with-open-file (stream path  :direction :output)
      (write-sequence string stream))
    (!equal (read-file path) string)))

(def-ui-test write-file-test
  (flet ((check-write (path string)
	   (write-file path string)
	   (let ((read-string (make-string (file-length path))))
	     (with-open-file (stream path)
	       (read-sequence read-string stream)
	       (!equal read-string string)))))
    (let ((path (path-from-string "file")))
      (check-write path (lines "one line" "and another"))
      (check-write path "other line"))))

;;
;;copying test
;;

(def-ui-test simple-copying
  (let ((source-path (path-from-string "file"))
	(dest-path (path-from-string "other.file")))
    (write-file source-path "this is a test file")
    (copy-file source-path dest-path)
    (?file-copied source-path dest-path)))

(def-ui-test copying-when-dest-exists
  (let ((source-path (path-from-string "a.file"))
	(dest-path (path-from-string "other.file")))
    (write-file source-path "a source")
    (write-file dest-path "other source")
    (?condition (copy-file source-path dest-path)
		file-already-exists-error (filesystem-error-path dest-path :test path=))))

(def-ui-test copying-to-self-error
  (let ((path (path-from-string "a.file")))
    (write-file path "some data")
    (?condition (copy-file path path)
		file-already-exists-error (filesystem-error-path path :test path=))))

(def-ui-test different-filesystems-copying
  (let ((fs2 (make-virtual-filesystem)))
    (let ((source-path (path-from-string "file"))
	  (dest-path (path-from-string "other.file" :fs fs2)))
      (write-file source-path "some test data")
      (copy-file source-path dest-path)
      (?file-copied source-path dest-path))))

(def-ui-test no-source-error
  (let ((source-path (path-from-string "file"))
	(dest-path (path-from-string "other.file")))
    (?condition (copy-file source-path dest-path)
		file-does-not-exist-error (filesystem-error-path source-path :test path=))))

(def-ui-test no-correct-dest-error
  (let ((source (path-from-string "file"))
	(dest (path-from-string "other.file"))
	(dest-as-dir (path-from-string "other.file/")))
    (write-file source "some data for file")
    (make-directory dest-as-dir)
    (?condition (copy-file source dest)
		no-correct-path-error (filesystem-error-path dest :test path=))))

(def-ui-test copying-files-with-directory-path
  (let ((source (path-from-string "a.file"))
	(source-dir (path-from-string "a.dir/"))
	(dest (path-from-string "other.file"))
	(dest-dir (path-from-string "other.dir/")))
    (make-file source)
    (make-directory source-dir)
    (?condition (copy-file source dest-dir)
		wrong-path-error (wrong-path-error-path dest-dir :test path=))
    (?condition (copy-file source-dir dest)
		wrong-path-error (wrong-path-error-path source-dir :test path=))))

;;
;;copying directories
;;

(def-ui-test simple-copying
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (make-directory source)
    (copy-directory source dest)
    (?directory-copied source dest)))

(def-ui-test copying-directory-when-dest-exists
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (make-directory source)
    (make-directory dest)
    (?condition (copy-directory source dest)
		directory-already-exists-error (filesystem-error-path dest :test path=)))) 

(def-ui-test copying-directory-to-self
  (let ((path (path-from-string "a.dir/")))
    (make-directory path)
    (?condition (copy-directory path path)
		directory-already-exists-error (filesystem-error-path path :test path=))))

(def-ui-test copying-without-source
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (?condition (copy-directory source dest)
		directory-does-not-exist-error (filesystem-error-path source :test path=))))

(def-ui-test no-correct-dest-error
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/sub.dir/")))
    (make-directory source)
    (?condition (copy-directory source dest)
		no-correct-path-error (filesystem-error-path dest :test path=))))

;;copying directories in different filesystems

(def-ui-test copying-directories-in-different-filesystesm
  (let ((fs (make-virtual-filesystem)))
    (let ((source (path-from-string "a.dir/"))
	  (dest (path-from-string "other.dir/" :fs fs)))
      (make-directory source)
      (copy-directory source dest)
      (?directory-copied source dest))))

(def-ui-test copying-directories-with-files
  (let ((fs (make-virtual-filesystem)))
    (let ((source (path-from-string "a.dir/"))
	  (source-file1 (path-from-string "a.dir/a.file"))
	  (source-file2 (path-from-string "a.dir/other.file"))
	  (dest (path-from-string "other.dir/" :fs fs)))
      (make-directory source)
      (write-file source-file1 "some data")
      (write-file source-file2 "other data")
      (copy-directory source dest)
      (?directory-copied source dest)
      (?file-copied source-file1 (path+ dest (path- source-file1 source)))
      (?file-copied source-file2 (path+ dest (path- source-file2 source))))))

(def-ui-test copying-directories-with-subdirs
  (let ((fs (make-virtual-filesystem)))
    (let ((source (path-from-string "a.dir/"))
	  (source-file1 (path-from-string "a.dir/a.file"))
	  (source-subdir (path-from-string "a.dir/sub.dir/"))
	  (source-file2 (path-from-string "a.dir/sub.dir/other.file"))
	  (dest (path-from-string "other.dir/" :fs fs)))
      (make-directory source)
      (make-directory source-subdir)
      (write-file source-file1 "some info")
      (write-file source-file2 "some other info")
      (copy-directory source dest)
      (?directory-copied source dest)
      (?file-copied source-file1 (path+ dest (path- source-file1 source)))
      (?directory-copied source-subdir (path+ dest (path- source-subdir source))) 
      (?file-copied source-file2 (path+ dest (path- source-file2 source))))))

;;
;;moving files
;;

(def-ui-test simple-file-moving
  (let ((source (path-from-string "a.file"))
	(dest (path-from-string "other.file"))
	(text "some data"))
    (write-file source text)
    (move-file source dest)
    (?file-moved source dest text)))

(def-ui-test moving-file-when-dest-exists
  (let ((source (path-from-string "a.file"))
	(dest (path-from-string "other.file")))
    (make-file source)
    (make-file dest)
    (?condition (move-file source dest) 
		file-already-exists-error (filesystem-error-path  dest :test path=))))

(def-ui-test moving-file-without-source
  (let ((source (path-from-string "a.file"))
	(dest (path-from-string "other.file")))
    (?condition (move-file source dest)
		file-does-not-exist-error (filesystem-error-path source :test path=))))
	  
(def-ui-test moving-file-to-no-correct-dest
  (let ((source (path-from-string "a.file"))
	(dest1 (path-from-string "a.dir/a.file"))
	(dest2 (path-from-string "other.file")))
    (make-file source)
    (make-directory (path-as-directory dest2))
    (mapc #'(lambda (path) 
	      (?condition (move-file source path) 
			  no-correct-path-error (filesystem-error-path path :test path=)))
	  (list dest1 dest2))))

(def-ui-test moving-with-directory-paths
  (let ((source (path-from-string "a.file"))
	(source-dir (path-from-string "a.dir/"))
	(dest (path-from-string "other.file"))
	(dest-dir (path-from-string "other.dir/")))
    (make-file source)
    (make-directory source-dir)
    (?condition (move-file source dest-dir)
		wrong-path-error (wrong-path-error-path dest-dir :test path=))
    (?condition (move-file source-dir dest)
		wrong-path-error (wrong-path-error-path source-dir :test path=))))

(def-ui-test moving-files-with-different-filesystems
  (let ((fs (make-virtual-filesystem)))
    (let ((source (path-from-string "a.file"))
	  (dest (path-from-string "a.file" :fs fs))
	  (text "some data to move"))
      (write-file source text)
      (move-file source dest)
      (?file-moved source dest text))))

;;
;;moving directories
;;

(def-ui-test moving-directories
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (make-directory source)
    (move-directory source dest)
    (?directory-moved source dest)))

(def-ui-test moving-directories-when-dest-exists
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (make-directory source)
    (make-directory dest)
    (?condition (move-directory source dest) 
		directory-already-exists-error (filesystem-error-path dest :test path=))))

(defmacro ?filesystem-error (expression error path)
  `(?condition ,expression ,error (filesystem-error-path ,path :test path=)))

(def-ui-test moving-directories-without-source
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/")))
    (?filesystem-error (move-directory source dest) directory-does-not-exist-error source)))

(def-ui-test moving-directories-with-no-correct-dest
  (let ((source (path-from-string "a.dir/"))
	(dest (path-from-string "other.dir/subdir/")))
    (make-directory source)
    (?filesystem-error (move-directory source dest) no-correct-path-error dest)))

(def-ui-test moving-directories-with-file-paths
  (let ((source (path-from-string "a.dir/"))
	(source-file (path-from-string "a.file"))
	(dest (path-from-string "other.dir/"))
	(dest-file (path-from-string "other.file")))
    (make-directory source)
    (make-file source-file)
    (?condition (move-directory source dest-file) 
		wrong-path-error (wrong-path-error-path dest-file :test path=))
    (?condition (move-directory source-file dest)
		wrong-path-error (wrong-path-error-path source-file :test path=))))

(def-ui-test moving-directory-to-different-filesystem
  (let ((fs (make-virtual-filesystem)))
    (let ((source (path-from-string "a.dir/"))
	  (dest (path-from-string "other.dir/" :fs fs)))
      (make-directory source)
      (move-directory source dest)
      (?directory-moved source dest))))


