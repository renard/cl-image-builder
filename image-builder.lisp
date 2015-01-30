;;;;
;;;; This package helps you to create custom standalone Common Lisp images
;;;; and to compile your project into a standalone app.
;;;;
;;;; Basically you have to define the *image-builder-config* and run (from
;;;; shell)
;;;;
;;;; - with SBCL:
;;;;     sbcl --no-sysinit --no-userinit --load image-builder.lisp
;;;; - with CCL
;;;;    ccl64  -n -l image-builder.lisp
;;;;

(require :asdf)

(asdf:defsystem #:image-builder
  :description "Build standalone Common Lisp images and apps"
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :components nil)

(defpackage #:image-builder
  (:use #:cl)
  (:export
   #:*quicklisp-setup-url*
   #:*build-directory*
   #:*quicklisp-bootstrap-file*
   #:*quicklisp-directory*
   #:*quicklisp-local-projects-directory*
   #:*quicklisp-init-file*
   #:build-image))

(in-package #:image-builder)
  
(defparameter *quicklisp-setup-url*
  "http://beta.quicklisp.org/quicklisp.lisp"
  "URL from where to download QuickLisp.")

(defparameter *build-directory* #P"build/"
	"Root of build directory")

(defparameter *quicklisp-bootstrap-file*
  (merge-pathnames #P"quicklisp.lisp" *build-directory*)
  "Path to the quicklisp bootstrap file.")


(defparameter *quicklisp-directory* 
  (merge-pathnames #P"quicklisp/" *build-directory*)
  "Quicklisp intallation directory")

(defparameter *quicklisp-local-projects-directory* 
  (merge-pathnames #P"local-projects/" *quicklisp-directory*)
  "Quicklisp local projects directory")

(defparameter *quicklisp-init-file*
  (merge-pathnames #P"setup.lisp" *quicklisp-directory*)
  "Path to the quicklisp setup file.")

(defparameter *image-builder-config*
  #P".image-builder.lisp"
  "Image builder configuration file.")


(defun die(&key (return-code 0) message)
  "A simple portable exit function."
  (when message
    (format (if return-code *error-output* t) "~a~%" message))
  #+sbcl
  (sb-ext:exit :code return-code)
  #+ccl
  (ccl:quit return-code))


(defun install-quicklisp ()
  "Load Quicklisp from local build tree.

If Quicklisp *QUICKLISP-INIT-FILE* is not found, download it from
*QUICKLISP-SETUP-URL* into *QUICKLISP-DIRECTORY* and load it."
  (let ((asdf:*central-registry* (list *default-pathname-defaults*)))
    (if (probe-file *quicklisp-init-file*)
	(progn
	  (format t "Loading ~a~%" *quicklisp-init-file*)
	  (load *quicklisp-init-file*))
	(progn
	  (format t "Creating ~a~%" *quicklisp-directory*)
	  (ensure-directories-exist *quicklisp-directory*)
	  (asdf:run-shell-command
	   (format nil "curl -o ~a ~a"
		   *quicklisp-bootstrap-file* *quicklisp-setup-url*))
	  (load *quicklisp-bootstrap-file*)
	  (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART")
		   :path *quicklisp-directory*)))))


(defun clone-git (git-config)
  (let* ((url (getf git-config :url))
	 (directory (when url (pathname-name url)))
	 (target (when directory
		   (merge-pathnames directory *quicklisp-local-projects-directory*))))
    (when target
      (unless (probe-file target)
	(asdf:run-shell-command
	 (format nil "git clone ~a ~a" url target))))))


(defun load-project (projects)
  "Load quicklisp PROJECTS."
  (when projects
    (let ((asdf:*central-registry* (list *default-pathname-defaults*)))
      (funcall (intern "QUICKLOAD" "QUICKLISP") projects))))


(defun split-string-by-char (string &key (char #\:))
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (remove-if #'(lambda(x) (string= "" x))
	     (loop for i = 0 then (1+ j)
		   as j = (position char string :start i)
		   collect (subseq string i j)
		   while j)))


(defun string-to-function-symbol(string)
  (let ((elements (split-string-by-char (string-upcase string) :char #\:)))
    (when elements
      (apply #'intern (reverse elements)))))

(defun write-image (conf)
  "Write the image file according CONF directives."
  (let ((entry-function (string-to-function-symbol
			 (getf conf :entry-point))))
    #+sbcl
    (let ((args (append
		 (cons (format nil "~a.sbcl.exe" (getf conf :file-output))
		       (getf conf :options-sbcl))
		 (when entry-function
		   (list
		    :toplevel
		    #'(lambda()
			(funcall entry-function sb-ext:*posix-argv*)))))))
      (apply #'sb-ext:save-lisp-and-die args))
    
    #+ccl
    (let ((args (append
		 (cons (format nil "~a.ccl.exe" (getf conf :file-output))
		       (getf conf :options-ccl))
		 (when entry-function
		   (list :toplevel-function
			 #'(lambda()
			     (funcall entry-function
				      ccl:*command-line-argument-list*)))))))
      (apply #'ccl:save-application args))))

(defun build-image ()
  "Run the build image process. This is the entry point. This function is
automatically called when loading this lisp file.

You can run BUILD-IMAGE
- with SBCL:
    sbcl --no-sysinit --no-userinit --load image-builder.lisp
- with CCL
   ccl64 -n -l image-builder.lisp
"
  (let ((opts
	  (handler-case
	      (with-open-file (stream *image-builder-config*) (read stream))
	    (condition (c)
	      (die :return-code 1 :message (format nil "~a~%" c))))))
    (when opts
      (format t "Installing quicklisp~%")
      (install-quicklisp)
      (format t "Installing custom modules~%")
      (loop for git in (getf opts :custom-modules)
	    do (clone-git git))
      (format t "Installing local systems~%")
      (load-project (getf opts :local-projects))
      (format t "Saving runtime image~%")
      (write-image opts))
    ;; Shouldn't be reached except on parse error
    (die :return-code 2
	 :message "Couldn't find build-image configuration.")))

(build-image)
