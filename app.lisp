(ql:quickload :quickdocs-server)

(defpackage quickdocs.server.app
  (:use :cl)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:import-from :caveman.middleware.dbimanager
                :<caveman-middleware-dbimanager>)
  (:import-from :clack.middleware.backtrace
                :<clack-middleware-backtrace>)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :osicat
                :environment-variable)
  (:import-from :quickdocs.server.web
                :*web*)
  (:import-from :quickdocs.server.config
                :config
                :*static-directory*))
(in-package :quickdocs.server.app)

(symbol-macrolet ((appenv (environment-variable "APP_ENV")))
  (unless appenv
    (setf appenv "default")))

(builder
 (<clack-middleware-static>
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/html/|/robot\\.txt$|/favicon.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (getf (config) :error-log)
     (make-instance '<clack-middleware-backtrace>
                    :output (getf (config) :error-log))
     nil)
 <clack-middleware-session>
 (if (getf (config) :databases)
     (make-instance '<caveman-middleware-dbimanager>
                    :database-settings (config :databases))
     nil)
 *web*)
