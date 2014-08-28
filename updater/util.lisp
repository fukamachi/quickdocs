(in-package :cl-user)
(defpackage quickdocs.updater.util
  (:use :cl)
  (:import-from :flexi-streams
                :octets-to-string
                :with-output-to-sequence)
  (:import-from :alexandria
                :copy-stream
                :with-gensyms
                :read-file-into-string)
  (:import-from :lparallel.kernel
                :*kernel*
                :make-channel
                :make-kernel
                :submit-task
                :receive-result
                :end-kernel))
(in-package :quickdocs.updater.util)

(cl-annot:enable-annot-syntax)

@export
(defun run-in-process (command &key (output *standard-output*) (error *error-output*) timeout)
  (let ((process
          (sb-ext:run-program "/bin/sh" `("-c" ,(format nil "~:[~;~:*timeout ~D ~]shly~:[~;~:* -f ~A~] ~{'~S'~^ ~}"
                                                        timeout
                                                        (asdf::getenv "SHELLY_FILE_PATH")
                                                        command))
                              :output output
                              :error error
                              :wait t)))
    (sb-ext:process-exit-code process)))

(defvar *task-progress-lock*
  (lparallel.thread-util:make-lock "task-progress-lock"))
(defvar *task-progress-function* (lambda (&optional name)))

@export
(defun print-progress (&optional name)
  (declare (ignore name))
  (error "print-progress have to be called only in map-parallel."))

@export
(defmacro map-parallel (fn list &key (worker-count 4))
  (with-gensyms (channel elem all progress name format-string)
    `(let* ((lparallel:*kernel* (lparallel:make-kernel ,worker-count))
            (,channel (lparallel.kernel:make-channel))
            (,all (length ,list))
            (,format-string (format nil "~~&[~~~DD/~~~:*~DD]~~:[~~;~~:* ~~A~~]~~%"
                                    (length (princ-to-string ,all))))
            (,progress 0))
       (flet ((print-progress (&optional ,name)
                (lparallel.thread-util:with-lock-held (*task-progress-lock*)
                  (format t ,format-string
                          (incf ,progress)
                          ,all
                          ,name))))
         (loop for ,elem in ,list
               do (lparallel.kernel:submit-task ,channel ,fn ,elem))
         (prog1 (loop repeat ,all
                      collect (lparallel.kernel:receive-result ,channel))
           (lparallel.kernel:end-kernel))))))

@export
(defun use-dist-version (dist-version &key (prompt t))
  (let ((dist (ql-dist:dist "quicklisp")))
    (when (string= (ql-dist:version dist) dist-version)
      (return-from use-dist-version t))

    (let* ((versions (ql-dist:available-versions dist))
           (version (assoc dist-version versions
                           :test #'string=)))
      (unless version
        (error "Dist \"quicklisp\" version ~A is not available."
               dist-version))

      (ql-dist:install-dist (cdr version) :prompt prompt :replace t))))

;;
;; README

;; for plain text
(defun plain-readme (file)
  (concatenate 'string "<pre>" (read-file-into-string file) "</pre>"))

;; for markdown
(defparameter *pandoc-path* "pandoc")

(defun pandoc-readme (file)
  (with-output-to-string (s)
    (sb-ext:run-program "/bin/sh" `("-c" ,(format nil "timeout 10 ~A ~A" *pandoc-path* file))
                        :output s)))

;; for org-mode
(defparameter *emacs-path* "/usr/bin/emacs")
(defun org2html-readme (file)
  (with-output-to-string (s)
    (sb-ext:run-program "/bin/sh" `("-c" ,(format nil "timeout 10 bin/org2html ~A ~A " *emacs-path* file))
                        :output s)))

;; for reStructuredText
(defparameter *rst2html-path* "rst2html.py")
(defun rst2html-readme (file)
  (with-output-to-string (s)
    (sb-ext:run-program "/bin/sh" `("-c" ,(format nil "timeout 10 ~A ~A" *rst2html-path* file))
                        :output s)))
