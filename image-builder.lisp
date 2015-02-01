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

;; (asdf:disable-output-translations)
;; asdf:system-relative-pathname

(asdf:defsystem #:image-builder
  :description "Build standalone Common Lisp images and apps"
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :components nil)

(defpackage #:image-builder
  (:use #:cl)
  (:export
   ;; #:*quicklisp-setup-url*
   ;; #:*build-directory*
   ;; #:*quicklisp-bootstrap-file*
   ;; #:*quicklisp-directory*
   ;; #:*quicklisp-local-projects-directory*
   ;; #:*quicklisp-init-file*
   ;;#:load-configuration
   #:build-image
   #:upgrade
   ;;#:read-asdf-definition
   ;;#:get-asdf-system-files
   ))

(in-package #:image-builder)


(defparameter *quicklisp-bootstrap-file* #P"quicklisp.lisp"
  "Path to the quicklisp bootstrap file.")

(defparameter *quicklisp-directory* #P"quicklisp/"
  "Quicklisp intallation directory")

(defparameter *quicklisp-local-projects-directory* 
  (merge-pathnames #P"local-projects/" *quicklisp-directory*)
  "Quicklisp local projects directory")

(defparameter *quicklisp-init-file*
  (merge-pathnames #P"setup.lisp" *quicklisp-directory*)
  "Path to the quicklisp setup file.")


(defparameter *default-build-dir* #P"build/"
	"Root of build directory")

(defparameter *default-quicklisp-setup-url*
  "http://beta.quicklisp.org/quicklisp.lisp"
  "URL from where to download QuickLisp.")


(defstruct configuration
  ""
  (asdf-file)
  (packages)
  (entry-point)
  (output-file)
  (options
   #+sbcl '(:purify t :executable t :compression t)
   #+ccl '(:error-handler :quit :prepend-kernel t))
  (custom-systems)
  (build-dir *default-build-dir*)
  (quicklisp-url *default-quicklisp-setup-url*))

(defstruct custom-system
  url
  method)


(defun load-configuration(file)
  "Load IMAGE-BUILDER configuration from FILE and return a CONFIGURATION
structure."
  (with-open-file (stream file :external-format :utf-8)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (apply #'make-configuration (read-from-string data)))))

(defun quicklisp-installedp (build-dir
			     &key (setup-file *quicklisp-init-file*))
 "Test if quicklisp is installed in BUILD-DIR by probing the existence of
SETUP-FILE."
  (probe-file (merge-pathnames setup-file
			       ;; (pathname-as-directory
				build-dir)))

(defun install-or-load-quicklisp
    (&key
       (url *default-quicklisp-setup-url*)
       (build-dir *default-build-dir*)
       (setup-file *quicklisp-init-file*)
       (quicklisp-bootstrap *quicklisp-bootstrap-file*))
  
  (let* ((ql-init (merge-pathnames setup-file build-dir))
	 (ql-dir (pathname (directory-namestring ql-init)))
	 (ql-bootstrap (merge-pathnames quicklisp-bootstrap build-dir)))

    ;; Download quicklisp bootstrap
    (if (quicklisp-installedp build-dir :setup-file setup-file)
	(progn
	  (format t "Loading Quicklisp from ~a~%" ql-init)
	  (load ql-init))
	(progn
	  (format t "Installing Quicklisp to ~a~%" ql-dir)
	  (ensure-directories-exist ql-dir)
	  (asdf:run-shell-command
	   (format nil "curl -o ~a ~a" ql-bootstrap url))
	  (load ql-bootstrap)
	  ;; Delete quicklisp from memory before installing it.
	  (when (find-package '#:ql)
	    (delete-package '#:ql))
	  ;; Bootstrap Quicklisp
	  (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART")
		   :path ql-dir)))))

(defun install-system-git (system target)
  (let* ((url (custom-system-url system))
	 (directory (when url (pathname-name url)))
	 (target (when directory
		   (merge-pathnames directory target))))
    (when target
      (unless (probe-file target)
	(asdf:run-shell-command
	 (format nil "git clone ~a ~a" url target))))))

(defun install-custom-systems (custom-systems
			       &key (build-dir *default-build-dir*)
				 (local-project *quicklisp-local-projects-directory*))
  (let ((lp-dir (merge-pathnames local-project build-dir)))
    (loop for system in custom-systems
	  for syst = (apply #'make-custom-system system)
	  do (progn
	       (cond
		 ((eq :git (custom-system-method syst))
		  (install-system-git syst lp-dir)))))))


(defun load-packages (packages)
  "Load quicklisp PROJECTS."
  (when packages
    (let ((asdf:*central-registry* (list *default-pathname-defaults*)))
      (funcall (intern "QUICKLOAD" "QUICKLISP") packages))))


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


(defun write-image (&key entry-point file-output options)
  "Write the image file according CONF directives."
  (let ((entry-function (string-to-function-symbol entry-point)))
    #+sbcl
    (let ((args (append
		 (cons (format nil "~a.sbcl.exe" file-output) options)
		 (when entry-function
		   (list
		    :toplevel
		    #'(lambda()
			(funcall entry-function sb-ext:*posix-argv*)))))))
      ;;(format t "~S~%" args)
      (apply #'sb-ext:save-lisp-and-die args))
    
    #+ccl
    (let ((args (append
		 (cons (format nil "~a.ccl.exe" file-output) options)
		 (when entry-function
		   (list :toplevel-function
			 #'(lambda()
			     (funcall entry-function
				      ccl:*command-line-argument-list*)))))))
      ;;(format t "~S~%" args)
      (apply #'ccl:save-application args))))



(defun build-image (&key
		      (config-file #P".image-builder.lisp")
		      (install-quicklisp t)
		      (custom-systems t)
		      (load-packages t)
		      )
  (let ((conf (load-configuration config-file)))
    ;;
    (when install-quicklisp
      (install-or-load-quicklisp
       :url (configuration-quicklisp-url conf)
       :build-dir (configuration-build-dir conf)))

    (when custom-systems
      (install-custom-systems (configuration-custom-systems conf)))

    (when load-packages
      (load-packages (configuration-packages conf)))

    (write-image
     :entry-point (configuration-entry-point conf)
     :file-output (configuration-output-file conf)
     :options (configuration-options conf))))




(defun read-asdf-definition (asdf-file)
  "Get all ASDF definition from ASDF-FILE"
  (format t "Reading ASDF into ~A~%" asdf-file)
  (let* ((%dir  (directory-namestring asdf-file))
	 (directory (pathname (if (string= "" %dir) "." %dir)))
	 (definition (let* ((*default-pathname-defaults* directory))
		       (with-open-file (stream asdf-file :external-format :utf-8)
			 (let ((data (make-string (file-length stream))))
			   (read-sequence data stream)
			   data)))))
    
    (remove
     nil
     (loop for i below (length definition)
	   collect (multiple-value-bind (system pos)
		       (read-from-string definition nil nil :start i)
		     (setq i (+ i pos))
		     (when (listp system)
		       ;; Make sure all files would be readable.
		       (when (not (getf system :pathname))
			 (setq system (append system (list :pathname ""))))
		       
		       (setf (getf system :pathname)
			     (merge-pathnames directory
					      ;; on some systems
					      ;; *default-pathname-defaults*
					      ;; is not absolute.
					      (truename
					       *default-pathname-defaults*)))
		       ;; Do not keep depends-on
		       (setf (getf system :depends-on nil) nil)
		       (eval system)))))))


(defun get-asdf-system-files (system)
  "Get all CL files from SYSTEM."
  (loop for child in (asdf:component-children system)
	when (eq 'asdf:cl-source-file (type-of child))
	  collect (asdf::component-pathname child)
	when (eq 'asdf:module (type-of child))
        nconc (get-asdf-system-files child)))

(defun get-asdf-systems-files (systems)
  "Get all CL files from all SYSTEMS."
  (loop for system in systems
	nconc (get-asdf-system-files system))) 



(defun upgrade (&key (config-file #P".image-builder.lisp") verbose)
  (let* ((conf (load-configuration config-file))
	 (asdf-defs (read-asdf-definition (configuration-asdf-file conf))))
    
    (loop for file in (get-asdf-systems-files asdf-defs)
	  do (progn
	       (when verbose (format t "Loading ~a~%" file))
	       (load file)))
    
    (write-image
     :entry-point (configuration-entry-point conf)
     :file-output (configuration-output-file conf)
     :options (configuration-options conf))))

;; image-builder.lisp ends here
