(defpackage #:burning-filesystem
  (:use #:burning-lisp #:trivial-gray-streams)
  (:export path
	   make-path
	   path-root
	   path-filesystem
	   path-directory
	   path-filename
	   path-name
	   path-type
	   path=

	   filename
	   filename-name
	   filename-type

	   path-from-string
	   path-to-string

	   path-p
	   file-path-p
	   directory-path-p

	   parent-path
	   root-path
	   copy-path
	   path+
	   path-
	   path-as-file
	   path-as-directory
	   relative-path-p
	   absolute-path-p

	   path-exists-p
	   correct-path-p

	   make-file
	   make-directory
	   remove-file
	   remove-directory

	   list-directory
	   resolve-path

	   as-absolute-path
	   as-relative-path

	   home-directory
	   current-directory

	   open-file
	   with-open-file
	   file-length

	   read-file
	   write-file

	   copy-file
	   copy-directory
	   move-file
	   move-directory
	   make-temporal-file
	   make-temporal-directory

	   filesystem-error
	   filesystem-error-path

	   file-lock-error
	   file-does-not-exist-error
	   directory-does-not-exist-error
	   file-already-exists-error
	   directory-already-exists-error

	   no-correct-path-error
	   directory-not-empty-error

	   wrong-path-error
	   wrong-path-error-path

	   wrong-filename-error
	   wrong-filename-error-string

	   fs-path-from-string
	   fs-path-to-string
	   fs-as-file-path
	   fs-as-directory-path
	   fs-file-exists-p
	   fs-directory-exists-p
	   fs-list-directory
	   fs-current-directory
	   fs-home-directory
	   fs-delete-file
	   fs-delete-directory
	   fs-make-file
	   fs-make-directory
	   fs-open-file
	   fs-open-input-stream
	   fs-open-output-stream
	   fs-open-io-stream
	   fs-close-stream
	   fs-file-length

	   fs-copy-file
	   fs-copy-directory
	   fs-move-file
	   fs-move-directory

	   common-filesystem
	   *default-filesystem*

	   make-virtual-filesystem
	   vfs-cd))
