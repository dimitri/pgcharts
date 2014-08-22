(in-package #:pgcharts)

;;;
;;; Handle the command line interface
;;;
(defvar *version-string* "1.0"
  "Version string: X.0 are development versions.")

(defvar *options*
  (list (make-option :help    "-h" "--help")
        (make-option :version "-V" "--version")
        (make-option :config  "-c" "--config" #'set-config-filename t))
  "List of allowed options for the main pgcharts command line.")

(define-condition server-error ()
  ((uri         :initarg :uri    :reader server-error-uri)
   (status-code :initarg :status :reader server-error-status-code)
   (reason      :initarg :reason :reader server-error-reason)
   (body        :initarg :body   :reader server-error-body)))

(define-condition cli-error ()
  ((mesg   :initarg :mesg   :initform nil :reader cli-error-message)
   (detail :initarg :detail :initform nil :reader cli-error-detail)
   (hint   :initarg :hint   :initform nil :reader cli-error-hint)))

(defun main (argv)
  "The main entry point for the command-line interface."
  (multiple-value-bind (args opts)
      (process-argv-options argv)

    (when (member :help opts)
      (usage args :help t)
      (uiop:quit 0))

    (when (member :version opts)
      (format t "pgcharts version ~s~%" *version-string*)
      (format t "compiled with ~a ~a~%"
                  (lisp-implementation-type)
                  (lisp-implementation-version))
      (uiop:quit 0))

    ;; don't do anything when --help or --version were given
    (let ((match (find-command-function args)))
      (if match
          (destructuring-bind (fun args) match
            (handler-case
                (handler-bind ((warning
                                #'(lambda (c)
                                    (format t "WARNING: ~a~%" c)
                                    (muffle-warning))))
                  (apply fun args))
              (cli-error (e)
                (format t
                        "ERROR: ~a~%~@[DETAIL: ~a~%~]~@[HINT: ~a~%~]"
                        (cli-error-message e)
                        (cli-error-detail e)
                        (cli-error-hint e)))
              (server-error (e)
                (format t
                        "ERROR ~d ON ~a~%~@[REASON: ~a~%~]~@[BODY: ~a~%~]"
                        (server-error-status-code e)
                        (server-error-uri e)
                        (server-error-reason e)
                        (server-error-body e)))
              (condition (c)
                (if (member :debug opts)
                    (invoke-debugger c)
                    (format t "ERROR: ~a~%" c)))))
          (usage argv)))))


;;;
;;; Actual commands
;;;
(define-command (("setup") (&optional dburi))
    "setup the pgcharts database"
  (let ((*dburi*  (or dburi *dburi*)))
    (write-config)
    (ensure-model-is-installed)))

(define-command (("status") ())
    "get the status of the currently running server"
  (handler-case
      (format t "~a~%" (server-status))
    (condition (e)
      (error 'cli-error
             :mesg    "pgcharts is not running"
             :detail  (format nil "~a" e)))))

(define-command (("pid") ())
    "prints the PID of the server, if running"
  (format t "~a~%" (read-pid *pidfile*)))

(define-command (("stop") ())
    "stop the pgcharts web server"
  (kill-server))

(define-command (("start") ())
    "start the pgcharts web server"
  (check-setup)
  (let ((status (ignore-errors (server-status))))
    (unless (and status (string= "OK" status))
      (daemon:daemonize :output *logfile*
                        :error  *logfile*
                        :pidfile *pidfile*
                        :exit-parent t
                        :sigterm (lambda (sig)
                                   (declare (ignore sig))
                                   (stop-server)))
      (start-server :logs *logfile*)
      (loop :while *server-is-running*
         :do (sleep 1)))))

(define-command (("register") (dburi))
    "register a database"
  (register-db dburi))

(define-command (("config") ())
    "print current config"
  (format t "Current config file: ~s~%" *config-filename*)
  (with-open-file (config *config-filename*)
    (uiop:copy-stream-to-stream config *standard-output*)))

(define-command (("get") (key))
    "display current value for given configuration key"
  (print-config-value key))

(define-command (("set") (key &optional val))
    "edit configuration variables"
  (if val
      (setf (config-value key) val)
      (print-config-value key)))


;;;
;;; Support code for the previous commands
;;;
(defun print-config-value (key)
  "Print the value for KEY, print nothing in error cases."
  (let ((value (config-value key)))
    (when value
      (format t "~a~%" value))))

(defun server-status ()
  "Get the server status over HTTP."
  (multiple-value-bind (body status-code headers uri stream must-close reason)
      (drakma:http-request (format nil "http://localhost:~d/status" *listen-port*))
    (declare (ignore headers stream must-close))
    (if (= status-code 200)
        body
        (error 'server-error
               :uri uri :status status-code :reason reason :body body))))

(defun kill-server (&optional (sig "TERM"))
  "Send a signal to the server for it to stop"
  (when (kill-pid (read-pid *pidfile*) sig)
    (ignore-errors (delete-file *pidfile*))))

(defun register-db (dburi)
  "Register a new database server."
  (when (validate-dburi dburi)
    (destructuring-bind (name user pass host &key (port 5432))
        (parse-pgsql-connection-string dburi)
      (with-pgsql-connection (*dburi*)
        (make-dao 'db
                  :dbname name
                  :dbhost host
                  :dbport port
                  :dbuser user
                  :dbpass pass)))))

(defun check-setup ()
  "Signal a condition when the setup isn't ready for starting the service."
  (unless *dburi*
    (error 'cli-error
           :mesg   "dburi is unset"
           :detail "pgcharts need its own database"
           :hint   "run: pgcharts setup pgsql://user:pass@host/dbname"))

  (handler-case
      (validate-dburi *dburi*)
    (condition (e)
      (error 'cli-error
             :mesg   (format nil "~a" e)
             :detail (format nil "failed to connect to dburi ~s" *dburi*)
             :hint   (format nil "~a~%~a~%"
                             "createdb dbname; then set dburi:"
                             "run: pgcharts setup pgsql://user:pass@host/dbname"))))

  (unless (model-installed-p *dburi*)
    (error 'cli-error
           :mesg   "pgcharts database model isn't setup"
           :detail "pgcharts needs to install its own database model"
           :hint   "run: pgcharts setup")))
