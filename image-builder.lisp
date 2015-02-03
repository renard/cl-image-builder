;;;;
;;;; This package helps you to create custom standalone Common Lisp images
;;;; and to compile your project into a standalone app.
;;;;
;;;; Basically you have to define the *image-builder-config* and run (from
;;;; shell)
;;;;
;;;; - with SBCL:
;;;;     sbcl --no-sysinit --no-userinit --load image-builder.lisp --eval '(image-builder:build-image)'
;;;; - with CCL
;;;;    ccl64  -n -l image-builder.lisp --eval '(image-builder:build-image)'
;;;;

(require :asdf)

;; (asdf:disable-output-translations)
;; asdf:system-relative-pathname

(defpackage #:image-builder
  (:use #:cl)
  (:export
   #:build-image
   #:upgrade))

(in-package #:image-builder)


(defparameter *quicklisp-bootstrap-file* #P"quicklisp.lisp"
  "Path to the quicklisp bootstrap file relative to BUILD-DIR from the
CONFIGURATION.")

(defparameter *quicklisp-directory* #P"quicklisp/"
  "Quicklisp intallation directory relative to BUILD-DIR from the
CONFIGURATION.")

(defparameter *quicklisp-local-projects-directory* 
  (merge-pathnames #P"local-projects/" *quicklisp-directory*)
  "Quicklisp local projects directory relative to *QUICKLISP-DIRECTORY*.")

(defparameter *quicklisp-init-file*
  (merge-pathnames #P"setup.lisp" *quicklisp-directory*)
  "Path to the quicklisp setup file relative to *QUICKLISP-DIRECTORY*.")


(defparameter *default-build-dir*
  #P"build/"
  "Default root of build directory overridden by :BUILD-DIR in
the configuration.")

(defparameter *default-quicklisp-setup-url*
  "http://beta.quicklisp.org/quicklisp.lisp"
  "URL from where to download QuickLisp.")





(defstruct configuration
  "CONFIGURATION structure where (asterisk items are mandatory):

- (*) :ASDF-FILE path to your project ASDF definition file.

- :PACKAGES is a list of packages composing your project.

- :ENTRY-POINT is the function to call when the lisp image is loaded. Let
  this option blank if you want to create a custom lisp image.

- :OPTIONS list of option to be passed to SB-EXT:SAVE-LISP-AND-DIE or
  CCL:SAVE-APPLICATION.

- :CUSTOM-SYSTEMS list of CUSTOM-SYSTEMS to be downloaded in the Quiclisp
  local-projects folder.

- :BUILD-DIR directory where to install Quicklisp and all
  dependencies. Overrides *DEFAULT-BUILD-DIR*

- :QUICKLISP-URL override *DEFAULT-QUICKLISP-SETUP-URL*
"
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
  "Definition of a custom system where:

- :URL an URL from where to download the custom system.

- :METHOD which method to use to downlowd the custom system. Supported
  values are :GIT.
"
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
    "Install Quicklisp into BUILD-DIR of load SETUP-FILE if Quicklisp is
already installed into BUILD-DIR.

If Quicklisp is not installed yet, the QUICKLISP-BOOTSTRAP file is
downloaded from URL and loaded using curl."
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
		   :path ql-dir)
	  (funcall (intern "QUICKLOAD" "QUICKLISP") :uiop)))))

(defun install-system-git (system target)
  "Install SYSTEM into TARGET directory using git command."
  (let* ((url (custom-system-url system))
	 (directory (when url (pathname-name url)))
	 (target (when directory
		   (merge-pathnames directory target))))
    (when target
      (unless (probe-file target)
	(asdf:run-shell-command
	 (format nil "git clone ~a ~a" url target))))))

(defun install-custom-systems
    (custom-systems
     &key (build-dir *default-build-dir*)
       (local-project *quicklisp-local-projects-directory*))
  "Download every custom system defined in CUSTOM-SYSTEMS list in the
LOCAL-PROJECT directory of BUILD-DIR.

CUSTOM-SYSTEMS is a list of CUSTOM-SYSTEM structure.
"
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
  "Convert STRING to its symbol function representation."
  (let ((elements (split-string-by-char (string-upcase string) :char #\:)))
    (when elements
      (apply #'intern (reverse elements)))))


(defun write-image (&key entry-point file-output options)
  "Write the common lisp image of current running lisp instance.

If an ENTRY-POINT is defined, that function would be called when the
instance is loaded again. The ENTRY-POINT function is call with all command
line argument as a list parameter, thus you don't have to worry about how to
retrieve command line arguments. The ENTRY-POINT signature should be
something like:

  (defun main (argv) ... )

FILE-OUTPUT is the basename of the file to be written. The output file is
suffixed with the lisp implementation you use and \".exe\". I t would result
of \"FILE-OUTPUT.sbcl.exe\" or \"FILE-OUTPUT.ccl.exe\"

A list of OPTIONS can be passed to SB-EXT:SAVE-LISP-AND-DIE or
CCL:SAVE-APPLICATION."
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
  "Build a custom lisp image from configuration read from  CONFIG-FILE.

If INSTALL-QUICKLISP is NIL Quicklisp setup would be skipped. IF
CUSTOM-SYSTEMS is NIL none of custom system definition would be loaded. If
LOAD-PACKAGES is NIL, no package defined in the ASDF-FILE configuration
would be loaded.

If the build succeed the program exits to the shell."
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
  "Get all ASDF definition from ASDF-FILE as a list of ASDF:SYSTEM."
  (format t "Reading ASDF from ~A~%" asdf-file)
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
		     (when (and (listp system) (> (length system) 0 ))
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
  "For a given ASDF:SYSTEM SYSTEM, return a list of CL files declared in
the :COMPONENTS section."
  (loop for child in (asdf:component-children system)
	when (eq 'asdf:cl-source-file (type-of child))
	  collect (asdf::component-pathname child)
	when (eq 'asdf:module (type-of child))
        nconc (get-asdf-system-files child)))

(defun get-asdf-systems-files (systems)
  "Calls GET-ASDF-SYSTEM-FILES for each system of the SYSTEMS list of
ASDF:SYSTEM an return a list of all declared CL files."
  (loop for system in systems
	nconc (get-asdf-system-files system))) 


(defun upgrade (&key (config-file #P".image-builder.lisp") verbose)
  "Upgrade current image using CONFIG-FILE settings. If VERBOSE is T,
display file name being loaded."
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
