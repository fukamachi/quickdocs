(in-package :cl-user)
(defpackage quickdocs.updater.project
  (:use :cl
        :caveman2.db)
  (:import-from :quickdocs.updater.model.project
                :ql-release-version
                :project-homepage
                :find-readme
                :repos-url)
  (:import-from :quickdocs.updater.model.system
                :system-info-in-process)
  (:import-from :quickdocs.util
                :slurp-file)
  (:import-from :quickdocs.updater.util
                :use-dist-version
                :map-parallel
                :print-progress
                :run-in-process
                :pandoc-readme
                :org2html-readme
                :plain-readme)
  (:import-from :quickdocs.parser
                :parse-documentation)
  (:import-from :quickdocs.parser.util
                :with-retrying))
(in-package :quickdocs.updater.project)

(cl-annot:enable-annot-syntax)

@export
(defgeneric insert-record (db object &key))

@export
(defmethod insert-record (db (release ql-dist:release) &key)
  (delete-from db :project
    (where (:and (:= :ql_dist_version (ql-dist:version (slot-value release 'ql-dist:dist)))
                 (:= :name (ql-dist:project-name release)))))

  (insert-into db :project
    (set= :ql_dist_version (ql-dist:version (slot-value release 'ql-dist:dist))
          :name (ql-dist:project-name release)
          :release_version (ql-release-version release)
          :homepage_url (project-homepage release)
          :repos_url (repos-url (ql-dist:project-name release))
          :archive_url (ql-dist::archive-url release)))
  (insert-readme))

@export
(defun insert-readme (db release)
  (let* ((project-id (getf (select-one db :id
                                       (from :project)
                                       (where (:and (:= :ql_dist_version (ql-dist:version (slot-value release 'ql-dist:dist)))
                                                    (:= :name (ql-dist:project-name release))))
                                       (limit 1))
                           :|id|))
         (readme-files (find-readme release))
         (readme (and readme-files
                      (loop for file in readme-files
                         for content = (handler-case (slurp-file file)
                                         (flex:external-format-encoding-error ()
                                           nil))
                         when content
                              do (return (cons file content))))))
    (when (and readme project-id)
      (delete-from db :project_readme
                   (where (:= :project_id project-id)))
      (insert-into db :project_readme
        (set= :project_id project-id
              :filename (pathname-name (car readme))
              :raw (cdr readme)
              :converted (let* ((file (car readme))
                                (type (pathname-type file)))
                           (cond
                             ((or (null type)
                                  (string= "txt" type)
                                  (string= "text" type))
                              (plain-readme file))
                             ((or (string= "markdown" type)
                                  (string= "md" type))
                              (pandoc-readme file))
                             ((string= "org" type)
                              (org2html-readme file))
                             (t
                              (plain-readme file)))))))))

@export
(defmethod insert-record (db (ql-system ql-dist:system) &key project-id system-info)
  (insert-into db :system
    (set= :project_id project-id
          :name (slot-value ql-system 'ql-dist:name)
          :version (and system-info (getf system-info :version))
          :description (and system-info (getf system-info :description))
          :long_description (and system-info (getf system-info :long-description))
          :license (and system-info (getf system-info :license)))))

@export
(defun insert-release (db release)
  (handler-bind ((ql-dist:badly-sized-local-archive
                   #'(lambda (e)
                       (let ((restart (find-restart 'ql-dist:delete-and-retry e)))
                         (when restart
                           (invoke-restart restart))))))
    (ql-dist:ensure-installed release))

  (insert-record db release)
  (let* ((dist-version (ql-dist:version (ql-dist:dist release)))
         (project-id (getf (select-one db :id
                             (from :project)
                             (where (:and (:= :ql_dist_version dist-version)
                                          (:= :name (ql-dist:project-name release))))
                             (limit 1))
                           :|id|)))
    (loop for ql-system in (ql-dist:provided-systems release)
          do (let ((system-info (system-info-in-process (slot-value ql-system 'ql-dist:name))))
               (insert-record db ql-system :project-id project-id :system-info system-info)
               (unless system-info
                 (error "Failed to load ASDF file of ~A." (slot-value ql-system 'ql-dist:name)))
               (flet ((insert-person (system-id name type)
                        (insert-into db :system_author
                          (set= :system_id system-id
                                :author_name name
                                :type type))))
                 (let ((system-id
                         (getf (select-one db :id
                                 (from :system)
                                 (where (:and (:= :project_id project-id)
                                              (:= :name (slot-value ql-system 'ql-dist:name))))
                                 (limit 1))
                               :|id|))
                       (authors (getf system-info :author))
                       (maintainers (getf system-info :maintainer)))

                   ;; Author & Maintainer
                   (dolist (author (if (listp authors) authors (list authors)))
                     (insert-person system-id author "author"))
                   (dolist (maintainer (if (listp maintainers) maintainers (list maintainers)))
                     (insert-person system-id maintainer "maintainer")))))))
  (values))

@export
(defun update-dist-database (db &optional (dist-version
                                           (ql-dist:version (ql-dist:dist "quicklisp"))))
  (use-dist-version dist-version)

  (let (error-releases)

    (map-parallel
     #'(lambda (release)
         (print-progress (ql-dist:project-name release))
         (handler-case (insert-release db release)
           (error (e)
             (format *error-output* "~&~A~%" e)
             (push (ql-dist:project-name release) error-releases))))
     (ql-dist:provided-releases (ql-dist:dist "quicklisp")))

    (format t "~3&Error releases:~%~{ - ~A~^~%~}~%" (reverse error-releases))))

@export
(defun update-dist-readme-database (db &optional (dist-version
                                                  (ql-dist:version (ql-dist:dist "quicklisp"))))
  (use-dist-version dist-version)
  (let (error-releases)
    (map-parallel
     #'(lambda (release)
         (print-progress (ql-dist:project-name release))
         (handler-case (insert-readme db release)
           (error (e)
             (format *error-output* "~&~A~%" e)
             (push (ql-dist:project-name release) error-releases))))
     (ql-dist:provided-releases (ql-dist:dist "quicklisp")))

    (format t "~3&Error releases:~%~{ - ~A~^~%~}~%" (reverse error-releases))))

@export
(defun update-dependencies (db &optional (dist-version
                                          (ql-dist:version (ql-dist:dist "quicklisp"))))
  (use-dist-version dist-version)

  (let ((systems (mapcan
                  (lambda (release)
                    (ql-dist:provided-systems release))
                  (ql-dist:provided-releases (ql-dist:dist "quicklisp")))))
    (loop for system in systems
          for system-id = (getf (select-one db :system.id
                                  (from :system)
                                  (left-join :project :on (:= :system.project_id :project.id))
                                  (where (:and (:= :ql_dist_version dist-version)
                                               (:= :system.name (slot-value system 'ql-dist:name))))
                                  (limit 1))
                                :|id|)
          do (format t "~&~A~%" (slot-value system 'ql-dist:name))
             (if system-id
                 (loop for dependency in (ql-dist:required-systems system)
                       do (let ((dependency-id
                                  (getf (select-one db :system.id
                                          (from :system)
                                          (left-join :project :on (:= :system.project_id :project.id))
                                          (where (:and (:= :ql_dist_version dist-version)
                                                       (:= :system.name dependency)))
                                          (limit 1))
                                        :|id|)))
                            (cond
                              ((and dependency-id
                                    (not (select-one db :id
                                           (from :system_dependencies)
                                           (where (:and (:= :system_id system-id)
                                                        (:= :depends_system_id dependency-id)))
                                           (limit 1))))
                               (insert-into db :system_dependencies
                                 (set= :system_id system-id
                                       :depends_system_id dependency-id)))
                              (dependency-id
                               (warn "~A depends on ~A, but the dependency already exists."
                                     (slot-value system 'ql-dist:name)
                                     dependency))
                              (T
                               (warn "~A depends on ~A, but it isn't found."
                                     (slot-value system 'ql-dist:name)
                                     dependency)))))
                 (warn "~A isn't found"
                       (slot-value system 'ql-dist:name))))))

@export
(defun parse-dist-releases (data-dir &optional (dist-version
                                                (ql-dist:version (ql-dist:dist "quicklisp"))))
  (use-dist-version dist-version)

  (let ((data-parse-dir (pathname (format nil "~A/~A/parse/" data-dir dist-version)))
        (data-error-dir (pathname (format nil "~A/~A/error/" data-dir dist-version)))
        (systems (mapcan
                  (lambda (release)
                    (ql-dist:provided-systems release))
                  (ql-dist:provided-releases (ql-dist:dist "quicklisp")))))

    (ensure-directories-exist data-parse-dir)
    (ensure-directories-exist data-error-dir)

    (map-parallel
     (lambda (system)
       (let ((stdout (make-string-output-stream))
             (errout (make-string-output-stream))
             code)
         (setf code
               (run-in-process `(with-retrying 5 (parse-documentation ,(intern (ql-dist:name system)
                                                                               :keyword)))
                               :output stdout
                               :error errout
                               :timeout (* 5 60)))

         (with-open-file (out (merge-pathnames (ql-dist:name system)
                                               (if (= code 0)
                                                   data-parse-dir
                                                   data-error-dir))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
           (princ (get-output-stream-string (if (= code 0)
                                                stdout
                                                errout)) out))

         (print-progress (format nil "~A - ~:[not ok~;ok~]"
                                 (ql-dist:name system)
                                 (= code 0)))))
     systems
     :worker-count 1)))
