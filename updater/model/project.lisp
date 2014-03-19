(in-package :cl-user)
(defpackage quickdocs.updater.model.project
  (:use :cl)
  (:import-from :quickdocs.updater.model.system
                :system-info-in-process)
  (:import-from :fad
                :file-exists-p
                :list-directory)
  (:import-from :ppcre
                :regex-replace
                :create-scanner
                :scan-to-strings)
  (:import-from :yason
                :parse)
  (:import-from :drakma
                :http-request
                :url-encode)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :cl-base64
                :string-to-base64-string)
  (:import-from :alexandria
                :when-let)
  (:import-from :org.tfeb.hax.memoize
                :memoize-function
                :function-memoized-p)
  (:import-from :quickdocs.util
                :slurp-file))
(in-package :quickdocs.updater.model.project)

(cl-annot:enable-annot-syntax)

(defparameter *quicklisp-projects-directory*
              (asdf:system-relative-pathname :quickdocs-updater
               #P"modules/quicklisp-projects/"))

(defparameter *github-access-token* nil)

(defun github-api-headers ()
  (if *github-access-token*
      `(("Authorization" . ,(format nil "Basic ~A"
                                   (string-to-base64-string (format nil "~A:x-oauth-basic" *github-access-token*)))))
      nil))

(defun project-source-file (project-name)
  (let ((filepath
         (merge-pathnames (format nil "~A/source.txt" project-name)
                          *quicklisp-projects-directory*)))
    (and (fad:file-exists-p filepath)
         filepath)))

(defun project-repository-url (project-name)
  (when-let (source (project-source-file project-name))
    (let ((data (slurp-file source)))
      (if (ppcre:scan "^ediware-http" data)
          (format nil "http://weitz.de/files/~A.tar.gz"
                  (drakma:url-encode project-name :utf-8))
          (ppcre:scan-to-strings "\\S+?://\\S+" data)))))

(defun url-domain (url)
  (aref (nth-value 1 (ppcre:scan-to-strings "^[^:]+?://([^/]+)" url))
        0))

@export
(defun repos-url (project-name)
  (when-let (repos-url (project-repository-url project-name))
    (let ((domain (url-domain repos-url)))
      (values
       (cond
         ((or (string= domain "github.com")
              (string= domain "gitorious.org"))
          (concatenate 'string
                       "https"
                       (ppcre:scan-to-strings "://.+/[^\.]+" repos-url)))
         ((string= domain "bitbucket.org")
          repos-url))
       domain))))

(defun github-repos-api (project-url)
  (let ((match
            (nth-value 1
                       (ppcre:scan-to-strings "://[^/]+?/([^/]+?)/([^/]+)" project-url))))
    (format nil "https://api.github.com/repos/~A/~A"
            (aref match 0)
            (aref match 1))))

(defun bitbucket-repos-api (project-url)
  (let ((match
            (nth-value 1
                       (ppcre:scan-to-strings "://[^/]+?/([^/]+?)/([^/]+)" project-url))))
    (format nil "https://api.bitbucket.org/1.0/repositories/~A/~A"
            (aref match 0)
            (aref match 1))))

(defun request-homepage-url (api-url key &optional headers)
  (multiple-value-bind (body status)
      (drakma:http-request api-url
                           :additional-headers headers)
    (when (= status 200)
      (let ((homepage
             (gethash key
                      (yason:parse
                       (flex:octets-to-string body)))))
        (cond
          ((or (null homepage) (string= homepage "")) nil)
          ((ppcre:scan "^[^:]+://" homepage) homepage)
          (t (concatenate 'string "http://" homepage)))))))
(unless (function-memoized-p 'request-homepage-url)
  (memoize-function 'request-homepage-url :test #'equal))

@export
(defun project-homepage (release)
  (let* ((project-name (ql-dist:project-name release))
         (ql-primary-system (find-primary-system-in-release release))
         (system-homepage (and ql-primary-system
                               (getf (system-info-in-process (slot-value ql-primary-system 'ql-dist:name))
                                     :homepage))))
    (or system-homepage
        (multiple-value-bind (url domain)
            (repos-url project-name)
          (cond
            ((string= domain "common-lisp.net")
             (format nil "http://common-lisp.net/project/~A/"
                     (drakma:url-encode project-name :utf-8)))
            ((string= domain "weitz.de")
             (format nil "http://weitz.de/~A/"
                     (drakma:url-encode project-name :utf-8)))
            (t (when-let (args (cond
                                 ((string= domain "github.com")
                                  (list (github-repos-api url) "homepage"
                                        (github-api-headers)))
                                 ((string= domain "butbucket.org")
                                  (list (bitbucket-repos-api url) "website"))))
                 (apply #'request-homepage-url args))))))))

(let ((prefix-scanner (ppcre:create-scanner "^cl-")))
  @export
  (defun find-primary-system-in-release (release)
    (flet ((emit-prefix (name)
             (ppcre:regex-replace prefix-scanner name "")))
      (let ((project-name (emit-prefix (ql-dist:project-name release))))
        (find-if
         #'(lambda (s)
             (string= (emit-prefix (ql-dist:name s))
                      project-name))
         (ql-dist:provided-systems release))))))

@export
(defmethod ql-release-version ((release ql-dist:release))
  (when-let (match
                (nth-value 1
                           (ppcre:scan-to-strings "beta\\.quicklisp\\.org/archive/[^/]+/([^/]+)" (slot-value release 'ql-dist::archive-url))))
    (aref match 0)))

@export
(defgeneric find-readme (object))

(defun find-readme-from-directory (directory)
  (remove-if-not
   #'(lambda (path)
       (let ((filename (file-namestring path)))
         (and (>= (length filename) 6)
              (string= "README" (subseq filename 0 6)))))
   (fad:list-directory directory)))

(defmethod find-readme ((system asdf:system))
  (find-readme-from-directory
   (slot-value system 'asdf::absolute-pathname)))

(defmethod find-readme ((system ql-dist:system))
  (find-readme-from-directory
   (fad:pathname-directory-pathname (ql-dist::installed-asdf-system-file system))))

(defmethod find-readme ((project ql-dist:release))
  (find-readme (car (ql-dist:provided-systems project))))
