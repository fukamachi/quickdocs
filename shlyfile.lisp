;; -*- mode: common-lisp -*-

(defparameter *shlyfile-pathname* (or *load-pathname* *compile-file-pathname*))

(let ((ql-home (probe-file
                (merge-pathnames #P"quicklisp/"
                                 (directory-namestring *shlyfile-pathname*)))))
  (unless ql-home
    (error "Local quicklisp home directory is not found."))

  (setf ql:*quicklisp-home* ql-home))

(ql:quickload '(:quickdocs-server :quickdocs-updater :split-sequence :dbi :dbd-mysql :osicat :caveman2-db))

(import 'split-sequence:split-sequence)
(import 'quickdocs.server.config:config)
(import 'caveman.middleware.dbimanager:connect-db)
(use-package :caveman2.db)

(unless (osicat:environment-variable "APP_ENV")
  (setf (osicat:environment-variable "APP_ENV") "default"))
(setf (osicat:environment-variable "SHELLY_FILE_PATH") (princ-to-string *shlyfile-pathname*))

#+sbcl(setf sb-impl::*default-external-format* :utf-8)
#+sbcl(setf sb-alien::*default-c-string-external-format* :utf-8)

;; XXX
(setf caveman.middleware.dbimanager::*dbi-manager*
      (make-instance 'caveman.middleware.dbimanager::dbi-manager
                     :database-settings (config :databases)))

(defun start (&key (port 8080)
                   (server :fcgi)
                   (debug nil))
  (flet ((start (&rest args)
           (apply #'quickdocs.server:start
                  :debug debug :server server args)))
    (let ((server-starter-port (asdf::getenv "SERVER_STARTER_PORT")))
      (if server-starter-port
          (destructuring-bind (port fd)
              (split-sequence #\=
                              (car (split-sequence #\; server-starter-port :count 1)))
            (start :port (parse-integer port)
                   :fd (parse-integer fd)))
          (start :port port)))))

(defun insert-release (release-name &optional (dist-version
                                               (ql-dist:version (ql-dist:dist "quicklisp"))))
  (quickdocs.updater.util:use-dist-version dist-version)
  (quickdocs.updater.project:insert-release
   (connect-db)
   (ql-dist:find-release release-name)))

(defun update-project-readme-database (&optional (dist-version
                                           (ql-dist:version (ql-dist:dist "quicklisp"))))
  (quickdocs.updater.project:update-dist-readme-database (connect-db) dist-version))

(defun update-project-database (&optional (dist-version
                                           (ql-dist:version (ql-dist:dist "quicklisp"))))
  (quickdocs.updater.project:update-dist-database (connect-db) dist-version))

(defun update-dependencies (&optional (dist-version
                                       (ql-dist:version (ql-dist:dist "quicklisp"))))
  (quickdocs.updater.project:update-dependencies (connect-db) dist-version))

(defun update-cliki-informations (&optional (dist-version
                                             (ql-dist:version (ql-dist:dist "quicklisp"))))
  (quickdocs.updater.cliki:update-cliki-informations (connect-db) dist-version))

(defun update-database (&optional (dist-version
                                   (ql-dist:version (ql-dist:dist "quicklisp"))))
  (format t "~3&Updating projects.~%")
  (update-project-database dist-version)
  (format t "~3&Updating dependencies.~%")
  (update-dependencies dist-version)
  (format t "~3&Updating informations.~%")
  (update-cliki-informations dist-version))

(defun parse-dist (&optional (dist-version
                              (ql-dist:version (ql-dist:dist "quicklisp"))))
  (format t "~3&Parsing releases.~%")
  (quickdocs.updater.project:parse-dist-releases
   (merge-pathnames #P"data/"
                    (fad:pathname-directory-pathname *shlyfile-pathname*))
   dist-version))

(defun update-dist (&optional (dist-version
                               (ql-dist:version (ql-dist:dist "quicklisp"))))
  (update-database dist-version)
  (parse-dist dist-version)

  (update (connect-db) :preference
    (set= :value dist-version)
    (where (:= :name "ql-dist-version"))))
