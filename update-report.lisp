;;;; update-report.lisp

(defpackage #:update-report
  (:use #:cl)
  (:shadowing-import-from
   #:ql-dist
   #:name
   #:update-release-differences
   #:find-release
   #:relative-to
   #:system-files
   #:ensure-installed))

(in-package #:update-report)

(defvar *project-base* #p"~/src/quicklisp-projects/")

(defun project-source-file (project)
  (let ((file
         (merge-pathnames (make-pathname :directory
                                         (list :relative project)
                                         :name "source"
                                         :type "txt")
                          *project-base*)))
    (truename file)))

(defun project-info (project)
  (let ((file (project-source-file project)))
    (with-open-file (stream file)
      (let ((raw-info (read-line stream)))
        (destructuring-bind (type location &rest extra)
            (ql-util:split-spaces raw-info)
          (list* :type type :location location
                 (when extra
                   (list :extra extra))))))))

(defparameter *guess-website-patterns*
  '(("//github.com/(.*)\\.git" "https://github.com/" 0 "/")))

(defun substitute-if-matches (regex target substitution)
  (multiple-value-bind (start end anchor-starts anchor-ends)
      (ppcre:scan regex target)
    (labels ((anchor-substring (index)
               (subseq target
                       (aref anchor-starts index)
                       (aref anchor-ends index)))
             (maybe-substitute (object)
               (etypecase object
                 (string object)
                 (integer (anchor-substring object)))))
      (when (and start end)
        (format nil "~{~A~}" (mapcar #'maybe-substitute substitution))))))

(defun guess-website-by-location-pattern (location)
  (dolist (spec *guess-website-patterns*)
    (destructuring-bind (regex . substitution)
        spec
      (let ((substitution (substitute-if-matches regex
                                                 location
                                                 substitution)))
        (when substitution
          (return substitution))))))

(defun guess-website (project)
  (let ((info (project-info project)))
    (destructuring-bind (&key type location extra)
        info
      (declare (ignore type extra))
      (or (guess-website-by-location-pattern location)
          (error "Cannot guess website for ~A from ~S"
                 project
                 info)))))

(defun defsystem-form-info (defsystem)
  (list :name (string-downcase (second defsystem))
        :license (or (getf defsystem :license)
                     (getf defsystem :licence))
        :description (getf defsystem :description)
        :homepage (getf defsystem :homepage)
        :author (getf defsystem :author)
        :version (getf defsystem :version)))

(defun system-file-info (system-file)
  "Read SYSTEM-FILE and look for a DEFSYSTEM form matching its
  pathname-name. Return the interesting properties of the
  form (license, description, perhaps more) as a plist."
  (with-open-file (stream system-file)
    (let* ((*load-truename* (truename system-file))
           (*load-pathname* *load-truename*))
      (loop with file-name = (pathname-name system-file)
            for form = (read stream nil stream)
            until (eq form stream)
            when (and (consp form)
                      (string-equal (first form) "DEFSYSTEM")
                      (string-equal (second form) file-name))
            return (defsystem-form-info form)))))

(defun test-system-p (pathname)
  (search "-test" (namestring pathname)))

(defun project-system-files (project)
  (let ((release (find-release project)))
    (unless release (error "Could not find a release for ~A" project))
    (ensure-installed release)
    (loop for file in (system-files release)
          unless (test-system-p file)
          collect (truename (relative-to release file)))))

(defun primary-system-file (project)
  (let ((system-files (project-system-files project)))
    (cond ((null (rest system-files))
           (first system-files))
          ((find project (project-system-files project)
                 :test 'string-equal
                 :key 'pathname-name))
          (t
           nil))))

(defvar *test-new-releases*
  '("chirp" "cl-flowd" "defvariant" "delta-debug" "random"))

(defun flatten-string (string)
  "Return STRING with all newlines and other whitespace converted to
  spaces."
  (ppcre:regex-replace-all "\\s+" string " "))

(defun cliki-encode (string)
  (setf string (substitute #\_ #\Space string))
  (drakma:url-encode string :utf-8))

(defun cliki-link (project)
  (format nil "http://cliki.net/~A" (cliki-encode project)))

(defun primary-system-info (project)
  (let* ((system (primary-system-file project))
         (info (and system (system-file-info system))))
    info))

(defun primary-system-property (project indicator)
  (getf (primary-system-info project) indicator))

(defun primary-system-website (project)
  (primary-system-property project :homepage))

(defun primary-system-description (project)
  (let ((description (primary-system-property project :description)))
    (when (and (stringp description)
               (plusp (length description)))
      (flatten-string description))))

(defun primary-system-license (project)
  (primary-system-property project :license))

(defun project-website (project)
  (or (primary-system-website project)
      (guess-website project)
      (cliki-link project)))

(defun github-properties (url)
  (ppcre:register-groups-bind (user repo)
      ("github.com/(.*?)/(.*?)(:?\\.git)?$" url)
    (list :user user
          :repo repo)))

(defvar *github-cache* (make-hash-table :test 'equalp))

(defun get-github-repo-info (project)
  (let* ((info (project-info project))
         (location (getf info :location))
         (github (github-properties location)))
    (when github
      (let ((repo-info-url (format nil "https://api.github.com/repos/~A/~A"
                                   (getf github :user)
                                     (getf github :repo))))
        (multiple-value-bind (body status)
            (drakma:http-request repo-info-url)
          (when (eql status 200)
            (yason:parse (babel:octets-to-string body :encoding :utf-8))))))))

(defun github-repo-info (project)
  (multiple-value-bind (info foundp)
      (gethash project *github-cache*)
    (if foundp
        info
        (setf (gethash project *github-cache*)
              (get-github-repo-info project)))))

(defun github-description (project)
  (let ((info (github-repo-info project)))
    (when info
      (values
       (gethash "description" info)))))

(defun project-description (project)
  (or (primary-system-description project)
      (github-description project)))

(defun project-license (project)
  (primary-system-license project))

(defun project-announce-info (project)
  (list :name project
        :license (or (project-license project)
                     "Unspecified")
        :website (project-website project)
        :description (or (project-description project)
                         "Unspecified")))

(defun new-release-html (project)
  (destructuring-bind (&key license website description &allow-other-keys)
      (project-announce-info project)
    (format nil "<a href='~A'>~A</a> &mdash; ~A &mdash; ~A"
            website project description license)))


(defun new-releases-html (projects)
  (format nil "<ul>~%~{ <li>~A</li>~%~}</ul>~%"
          (mapcar 'new-release-html projects)))

(defun new-releases-to-file (projects file)
  (with-open-file (stream file :direction :output
                          :if-exists :supersede)
    (write-string (new-releases-html projects) stream))
  (probe-file file))

(defun update-report* (old-dist new-dist)
  (multiple-value-bind (new updated removed)
      (ql-dist:update-release-differences old-dist new-dist)
    (format t "<p>New projects: ~A~%~%"
	    (new-releases-html (mapcar #'name new)))
    (format t "<p>Updated projects: ~{~A~^, ~}.~%~%"
	    (mapcar #'name (mapcar #'first updated)))
    (format t "<p>Removed projects: ~{~A~^, ~}."
	    (mapcar #'name removed))))
