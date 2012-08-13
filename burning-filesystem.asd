(in-package :asdf)

(defsystem #:burning-filesystem
    :description "Cross-implementation library to work with filesystems."
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "base-filesystem")
		 (:file "user-interface")
		 (:file "common-filesystem")
		 (:module "vfs" 
			  :serial t
			  :components
			  ((:file "base")
			   (:file "file-stream"))))
    :depends-on (#:burning-lisp #:trivial-gray-streams))


