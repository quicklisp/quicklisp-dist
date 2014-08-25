;;;; dist-index.lisp

#|

 Dist update is something like

 (sync-mock)
 (defparameter *update* (merge-dists "quicklisp" "mock"))
 (setf (name *update*) "quicklisp" (version *update*) (date-for-today))
 (upload-new-releases *update*)
(progn
 (write-indexes *update* "/tmp/update/")
 (upload-distinfo-files *update* "/tmp/update/")
 (put-to-s3-url "/tmp/update/distinfo.txt"
                "http://beta.quicklisp.org/dist/quicklisp.txt"
                :overwrite t :content-type "text/plain")
 (write-dist-versions-file "quicklisp" "/tmp/update/quicklisp-versions.txt")
 (put-to-s3-url "/tmp/update/quicklisp-versions.txt" "http://beta.quicklisp.org/dist/quicklisp-versions.txt" :overwrite t :content-type "text/plain")
 (invalidate-metadata "quicklisp"))

|#


(in-package #:ql-dist)

(defvar *temp-alphabet* "abcdefghijklmnopqrstuvwxyz0123456890")

(defvar *dist-bucket* "beta.quicklisp.org")

(defun sync-mock ()
  (commando:with-posix-cwd (qmerge "dists")
    (commando:run "rm" "-rf" "mock")
    (commando:run "rsync" "-av" "darkhorse:quicklisp/dists/mock/" "mock")))

(defgeneric write-index-entry (object stream)
  (:documentation
   "Write an index entry for OBJECT to STREAM."))

(defmethod write-index-entry ((system system) stream)
  (format stream "~A ~A ~A~{ ~A~}~%"
          (name (release system))
          (system-file-name system)
          (name system)
          (required-systems system)))

(defmethod write-index-entry ((release release) stream)
  (format stream "~A ~A ~D ~A ~A ~A~{ ~A~}~%"
          (name release)
          (archive-url release)
          (archive-size release)
          (archive-md5 release)
          (archive-content-sha1 release)
          (prefix release)
          (system-files release)))

(defgeneric write-indexes (object base-directory)
  (:documentation "Write out indexes for OBJECT in BASE-DIRECTORY."))

(defmethod project-name ((system system))
  (project-name (release system)))

(defgeneric write-distinfo (object stream))

(defmethod write-distinfo ((dist dist) stream)
  (flet ((slot-record (accessor)
           (format stream "~A: ~A~%"
                   (string-downcase accessor)
                   (funcall accessor dist))))
    (dolist (accessor '(name version system-index-url release-index-url
                        archive-base-url canonical-distinfo-url
                        distinfo-subscription-url))
      (slot-record accessor))))

(defmethod write-indexes ((dist dist) base-directory)
  (let ((system-index (merge-pathnames "systems.txt" base-directory))
        (release-index (merge-pathnames "releases.txt" base-directory))
        (distinfo (merge-pathnames "distinfo.txt" base-directory)))
    (ensure-directories-exist base-directory)
    (with-open-file (stream distinfo :direction :output
                            :if-exists :rename-and-delete)
      (write-distinfo dist stream))
    (flet ((write-index-file (file list header)
             (setf list (sort list #'string< :key #'project-name))
             (with-open-file (stream file :direction :output
                                     :if-exists :rename-and-delete)
               (write-line header stream)
               (dolist (object list)
                 (write-index-entry object stream)))))
      (write-index-file system-index (provided-systems dist)
                        "# project system-file system-name [dependency1..dependencyN]")
      (write-index-file release-index (provided-releases dist)
                        "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]"))))

(defclass merged-dist (dist)
  ((source-dist
    :initarg :source-dist
    :accessor source-dist)))

(defclass merged-release (release) ())

(defmethod local-archive-file ((release merged-release))
  (let ((default (call-next-method)))
    (or (probe-file default)
	(let* ((source (source-dist (dist release)))
	       (original-release (find-release-in-dist (name release) source)))
	  (unless original-release
	    (error "No orignal release for ~A" release))
	  (let ((original-file (probe-file (local-archive-file original-release))))
	    (unless original-file
	      (error "No original file for ~A" original-release))
	    original-file)))))

(defgeneric copy-instance (object &rest initargs))

(defmethod copy-instance ((system system) &rest initargs)
  (apply #'make-instance
         (class-of system)
         (append initargs
                 (list
                  :name (name system)
                  :system-file-name (system-file-name system)
                  :release (release system)
                  :dist (dist system)
                  :required-systems (required-systems system)))))

(defmethod copy-instance ((release release) &rest initargs)
  (let ((copy (apply #'make-instance
                     (class-of release)
                     (append initargs
                             (list
                              :project-name (project-name release)
                              :dist (dist release)
                              :provided-systems (provided-systems release)
                              :archive-url (archive-url release)
                              :archive-size (archive-size release)
                              :archive-md5 (archive-md5 release)
                              :archive-content-sha1 (archive-content-sha1 release)
                              :prefix (prefix release)
                              :system-files (system-files release))))))
    (setf (provided-systems copy)
          (mapcar (lambda (system)
                    (copy-instance system
                                   :dist (dist copy)
                                   :release copy))
                  (provided-systems copy)))
    copy))

(defmethod find-system-in-dist (name (dist merged-dist))
  (dolist (release (provided-releases dist))
    (when (equalp name (name release))
      (return release))))

(defgeneric (setf find-release-in-dist) (new-release name dist))

(defmethod (setf find-release-in-dist) (new-release name (dist merged-dist))
  (let ((new (copy-instance new-release :dist dist))
        (old (gethash name (release-index dist))))
    (setf (gethash name (release-index dist))
          (copy-instance new-release :dist dist))
    (if old
	(setf (provided-releases dist)
	      (nsubstitute new old (provided-releases dist)))
	(setf (provided-releases dist)
	      (merge 'list
		     (list new)
		     (provided-releases dist)
		     #'string<
		     :key #'name)))))

(defun make-merged-dist (dist-name &key version name)
  (let ((dist (find-dist dist-name)))
    (provided-systems dist)
    (provided-releases dist)
    (initialize-release-index dist)
    (initialize-system-index dist)
    (change-class dist 'merged-dist
                  :name (or name (name dist))
                  :version (or version (version dist)))))

(defmethod provided-systems ((dist merged-dist))
  (sort
   (reduce #'append (mapcar #'provided-systems (provided-releases dist)))
   #'string<
   :key #'name))

(defmethod initialize-system-index ((dist merged-dist))
  (error "Not supported"))

(defmethod initialize-release-index ((dist merged-dist))
  (error "Not supported"))

(defgeneric merge-dists (source target &key keep))

(defun ordered-provided-systems (release)
  (sort (mapcar 'name (provided-systems release)) #'string<))

(defun release-equal (r1 r2)
  (and (equalp (ordered-provided-systems r1)
               (ordered-provided-systems r2))
       #+nil
       (= (archive-size r1)
	  (archive-size r2))
       (or (equalp (archive-md5 r1)
                   (archive-md5 r2))
           (equalp (archive-content-sha1 r1)
                   (archive-content-sha1 r2)))))

(defun explain-release-difference (r1 r2)
  (let ((ps1 (ordered-provided-systems r1))
        (ps2 (ordered-provided-systems r2))
        (m1 (archive-md5 r1))
        (m2 (archive-md5 r2))
        (s1 (archive-content-sha1 r1))
        (s2 (archive-content-sha1 r2))
	(size1 (archive-size r1))
	(size2 (archive-size r2))
        (name (project-name r1))
        (v1 nil)
        (v2 nil)
	(p1 (prefix r1))
	(p2 (prefix r2))
        (field nil))
    (cond ((not (equalp ps1 ps2))
           (setf v1 ps1 v2 ps2 field "provided systems"))
          ((string/= m1 m2)
           (setf v1 m1 v2 m2 field "MD5"))
          ((string/= s1 s2)
           (setf v1 s1 v2 s2 field "SHA1"))
	  ((/= size1 size2)
	   (setf v1 size1 v2 size2 field "file sizes"))
          (t (error "Releases don't differ!")))
    (format t "~S (~A -> ~A) differs in ~A: ~%    ~S~% vs ~S~%" name
	    p1 p2 field v1 v2)
    (when (equalp p1 p2)
      (warn "Prefixes (~A) match!" p1))))

(defmethod merge-dists ((source string) (target string) &key keep)
  (merge-dists (make-merged-dist source)
               (make-merged-dist target)
               :keep keep))

(defmethod merge-dists ((source merged-dist) (target merged-dist) &key keep)
  "Any releases in TARGET that match SOURCE are replaced with the
SOURCE release. TARGET should be the new release and SOURCE should be
the old stuff. Project names in the KEEP list are copied unchanged
from SOURCE to TARGET."
  (setf (source-dist target) source)
  (dolist (old-release (provided-releases source))
    (let* ((name (project-name old-release))
           (kept (member name keep :test 'string-equal))
           (new-release (find-release-in-dist name (if kept source
                                                       target))))
      (when (member name keep :test 'string-equal)
	(format t "~A is KEPT~%" name))
      (when (and new-release
                 (release-equal old-release new-release))
	#+nil
        (format t "~A is unchanged, keeping old~%" name)
        (setf (find-release-in-dist name target) old-release))
      (when (and new-release
                 (not (release-equal old-release new-release)))
        (explain-release-difference old-release new-release))
      (when (not new-release)
        (format t "~A will be REMOVED~%" name))))
  (setf (version target) (date-for-today))
  target)

(defgeneric reinitialize-urls (object))

(defmethod reinitialize-urls ((dist merged-dist))
  (let ((host *dist-bucket*)
        (name (name dist))
        (version (version dist)))
    (setf (system-index-url dist)
          (format nil "http://~A/dist/~A/~A/systems.txt" host name version))
    (setf (release-index-url dist)
          (format nil "http://~A/dist/~A/~A/releases.txt" host name version))
    (setf (canonical-distinfo-url dist)
          (format nil "http://~A/dist/~A/~A/distinfo.txt" host name version))
    (setf (distinfo-subscription-url dist)
          (format nil "http://~A/dist/~A.txt" host name))
    (dolist (release (provided-releases dist) (clrhash (release-index dist)))
      (multiple-value-bind (bucket key)
	  (s3-components (archive-url release))
	(setf (archive-url release)
		     (format nil "http://~A/~A" *dist-bucket* key))))))

(defmethod (setf name) :after (new-name (dist merged-dist))
  (reinitialize-urls dist))

(defmethod (setf version) :after (new-version (dist merged-dist))
  (reinitialize-urls dist))

(defun s3-components (url)
  (setf url (url url))
  (values (hostname url)
          (string-left-trim "/" (path url))))

(defun s3-object-exists (bucket key)
  (let ((http-status (nth-value 1 (zs3:head :bucket bucket :key key))))
    (<= 200 http-status 299)))

(defun s3-url-exists (url)
  (multiple-value-bind (bucket key)
      (s3-components url)
    (s3-object-exists bucket key)))

(defun put-to-s3-url (file url &key overwrite
                      (content-type "binary/octet-stream")
                      content-disposition)
  (multiple-value-bind (bucket key)
      (s3-components url)
    (let ((exists (s3-object-exists bucket key)))
      (when (or (not exists) overwrite)
        (zs3:put-file file bucket key
                      :public t
                      :content-type content-type
                      :content-disposition content-disposition)))))

(defun check-archive-size (release)
  (let ((archive (local-archive-file release)))
    (when (probe-file archive)
      (when (/= (archive-size release)
		(file-size archive))
	(error "Size mismatch on ~A" (name release))))))

(defun upload-new-releases (dist &key overwrite)
  (dolist (release (provided-releases dist))
    (unless (typep release 'merged-release)
      (change-class release 'merged-release))
    :retry
    (with-simple-restart (retry "Retry")
      (force-output)
      (let ((url (archive-url release))
	    (archive (local-archive-file release)))
	(multiple-value-bind (bucket key)
	    (s3-components url)
	  (if (and (s3-object-exists bucket key) (not overwrite))
	      (format t "~A already there~%" release)
	      (progn
		(when (probe-file archive)
		  (when (/= (archive-size release)
			    (file-size archive))
		    (error "Size mismatch on ~A" (name release)))
		  (format t "Uploading ~A~%"
			  release)
		  (put-to-s3-url (local-archive-file release)
				     (archive-url release)
				     :overwrite t))))))
      (go :done))
    (warn "Retrying")
    (go :retry)
    :done))

(defun check-new-releases (dist)
  (dolist (release (provided-releases dist))
    (force-output)
    (let ((url (archive-url release)))
      (multiple-value-bind (bucket key)
          (s3-components url)
        (unless (s3-object-exists bucket key)
          (warn "~A missing" release))))))

(defun upload-distinfo-files (dist base-directory &key overwrite)
  (flet ((url (file)
           (format nil "http://~A/dist/~A/~A/~A"
		   *dist-bucket*
                   (name dist)
                   (version dist)
                   (file-namestring file))))
    (dolist (file '("distinfo.txt" "releases.txt" "systems.txt"))
      (setf file (merge-pathnames file base-directory))
      (let ((url (url file)))
        (multiple-value-bind (bucket key)
            (s3-components url)
          (if (and (s3-object-exists bucket key) (not overwrite))
              (format t "~A already exists~%" url)
              (progn
                (format t "Uploading ~A~%" url)
                (put-to-s3-url file url
                               :content-type "text/plain"))))))))


;;;
;;; Creating/updating dist version file
;;;

(defun write-dist-versions-file (dist-name file)
  (flet ((key-version (string)
           (elt (split-spaces (substitute #\Space #\/ string))
                2)))
    (let ((keys (zs3:all-keys *dist-bucket*
                              :prefix (format nil "dist/~A/" dist-name))))
      (when (zerop (length keys))
        (error "No versions for ~A found!" dist-name))
      (with-open-file (stream file :direction :output
                              :if-exists :rename-and-delete)
        (map nil
             (lambda (key)
               (when (ppcre:scan "distinfo\\.txt$" (zs3:name key))
                 (let* ((name (zs3:name key))
                        (url (format nil "http://~A/~A"
				     *dist-bucket*
                                     name))
                        (version (key-version name)))
                   (format stream "~A ~A~%" version url))))
             keys)))))

(defun new-releases (old new)
  (set-difference (provided-releases new) (provided-releases old)
                  :key 'name :test 'string=))

(defun invalidate-metadata (dist-name)
  (let ((distribution (first (zs3:all-distributions))))
    (zs3:invalidate-paths distribution
                          (list (format nil "/dist/~A.txt" dist-name)
                                (format nil "/dist/~A-versions.txt"
                                        dist-name)))))

(defun date-for-today ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D"
            year month date)))



(defun temporary-pathname ()
  (let ((name (make-string 16)))
    (flet ((random-char ()
             (aref *temp-alphabet* (random (length *temp-alphabet*)))))
      (map-into name #'random-char))
    (qmerge (make-pathname :type nil
                           :name nil
                           :directory (list :relative "tmp"
                                            name)))))



(defmacro with-quicktemp-directory (&body body)
  "Create a new temporary directory in the Quicklisp tree and delete
  it when leaving the dynamic contour. Binds
  *DEFAULT-PATHNAME-DEFAULTS* to the temp directory."
  (let ((path (copy-symbol 'path)))
    `(let* ((,path (temporary-pathname))
            (*default-pathname-defaults* ,path))
       (ensure-directories-exist ,path)
       (unwind-protect
            (progn ,@body)
         (delete-directory-tree ,path)))))


(defun call-for-last-two-dists (dist-name fun &key skip)
  (let* ((versions (available-versions (dist dist-name)))
         (old-path #p"old/")
         (new-path #p"new/")
         (new-distinfo #p "new/distinfo.txt")
         (old-distinfo #p "old/distinfo.txt"))
    (when skip
      ;; This is necessary when I screw up and make two releases
      ;; between announcements.
      (setf (cdr versions) (nthcdr skip (cdr versions))))
    (destructuring-bind ((new-version . new-url)
                         (old-version . old-url))
        (subseq versions 0 2)
      (declare (ignore old-version new-version))
      (with-quicktemp-directory
        (ensure-directories-exist old-path)
        (ensure-directories-exist new-path)
        (fetch new-url new-distinfo)
        (fetch old-url old-distinfo)
        (let ((new (make-dist-from-file new-distinfo))
              (old (make-dist-from-file old-distinfo)))
          (setf (base-directory new) new-path)
          (setf (base-directory old) old-path)
          (funcall fun old new))))))

(defun description-report (systems file)
  (with-output-to-string (stream)
    (dolist (system systems)
      (let ((description (ql-cdb:lookup (name system) file)))
        (if description
            (format stream "~A - ~A~%" (name system) description)
            (write-line (name system) stream))))))

(defun update-report (dist-name)
  (let* ((dist (find-dist "mock"))
         (descriptions (relative-to dist "descriptions.cdb")))
    (multiple-value-bind (new updated removed)
        (call-for-last-two-dists dist-name #'update-release-differences)
      (if (probe-file descriptions)
          (format t "New projects: ~%~%~A~%~%"
                  (description-report new descriptions))
          (format t "New projects: ~{~A~^, ~}.~%~%"
                  (mapcar #'name new)))
      (format t "Updated projects: ~{~A~^, ~}.~%~%"
              (mapcar #'name (mapcar #'first updated)))
      (format t "Removed projects: ~{~A~^, ~}."
              (mapcar #'name removed)))))

(defun update-report* (old-dist new-dist)
  (multiple-value-bind (new updated removed)
      (update-release-differences old-dist new-dist)
    (format t "New projects: ~{~A~^, ~}.~%~%"
	    (mapcar #'name new))
    (format t "Updated projects: ~{~A~^, ~}.~%~%"
	    (mapcar #'name (mapcar #'first updated)))
    (format t "Removed projects: ~{~A~^, ~}."
	    (mapcar #'name removed))))

(defun make-alpha-update (&key keep (dist "mock"))
  (let ((update (merge-dists "quicklisp" dist :keep keep)))
    (setf (name update) "qlalpha"
	  (version update) (date-for-today))
    update))

(defun call-with-retrying (fun)
  (handler-bind
      ((error (lambda (condition)
		(when (find-restart 'retry condition)
		  (warn "Retrying after condition: ~A" condition)
		  (invoke-restart 'retry)))))
    (funcall fun)))

(defmacro with-retrying (&body body)
  `(call-with-retrying (lambda () ,@body)))

(defun s3-url-exists (url)
  (multiple-value-bind (bucket key)
      (s3-components url)
    (s3-object-exists bucket key)))

(defun sanity-check-dist-uploads (dist)
  (let ((releases (provided-releases dist)))
    (dolist (release releases)
      (unless (s3-url-exists (archive-url release))
	(warn "No archive file found for ~A" (name release))))))

(defun publish-alpha (&key keep (dist "mock"))
  (let* ((*dist-bucket* "alpha.quicklisp.org")
	 (update (make-alpha-update :keep keep :dist dist))
	 (zs3:*credentials* (zs3:file-credentials "~/.qlalpha")))
    (with-retrying
      (upload-new-releases update :overwrite nil)
      (write-indexes update "/tmp/qlalpha/")
      (upload-distinfo-files update "/tmp/qlalpha/")
      (put-to-s3-url "/tmp/qlalpha/distinfo.txt"
		     "http://alpha.quicklisp.org/dist/qlalpha.txt"
		     :overwrite t :content-type "text/plain")
      (write-dist-versions-file "qlalpha"
				"/tmp/qlalpha/qlalpha-versions.txt")
      (put-to-s3-url "/tmp/qlalpha/qlalpha-versions.txt"
		     "http://alpha.quicklisp.org/dist/qlalpha-versions.txt"
		     :overwrite t :content-type "text/plain"))
    update))

(defun alpha-problems (dist)
  (dolist (release (provided-releases dist))
    (let ((s3-url-exists-p (s3-url-exists (archive-url release)))
	  (file-exists-p (probe-file (local-archive-file release))))
      (when (and (not s3-url-exists-p)
		 (not file-exists-p))
	(print release)))))
