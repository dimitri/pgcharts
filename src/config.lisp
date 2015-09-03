(in-package #:pgcharts)

(defvar *config-filename* "~/.pgcharts.ini"
  "Where to store pgcharts configuration.")

(defparameter *dburi* nil
  "PostgreSQL database connection.")

(defparameter *listen-address* nil
  "Which address to listen to, defaults to nil, meaning *.")

(defparameter *listen-port* 9042
  "Port bound by the repository server, exposing the HTTP protocol.")

(defparameter *pidfile* "~/.pgcharts.pid"
  "pgcharts pid file")

(defparameter *logfile* "/tmp/pgcharts.log"
  "Main logfile for pgcharts")

(defparameter *document-root*
  (asdf:system-relative-pathname :pgcharts "web/"))

(defparameter *js-root*
  (asdf:system-relative-pathname :pgcharts "web/js"))

(defparameter *bootstrap-root*
  (asdf:system-relative-pathname :pgcharts "web/bootstrap-3.1.1-dist"))

(defparameter *images-root*
  (asdf:system-relative-pathname :pgcharts "web/images"))

(defparameter *highcharts-root*
  (asdf:system-relative-pathname :pgcharts "web/highcharts"))

(defparameter *codemirror-root*
  (asdf:system-relative-pathname :pgcharts "web/codemirror-4.5"))

(defparameter *d3js*
  (asdf:system-relative-pathname :pgcharts "web/d3js/d3.min.js"))

(defparameter *hallo-root*
  (asdf:system-relative-pathname :pgcharts "web/hallo"))

(defparameter *fontawesome-root*
  (asdf:system-relative-pathname :pgcharts "web/font-awesome-4.2.0"))

(defparameter *header-path*
  (asdf:system-relative-pathname :pgcharts "web/header.html"))

(defparameter *footer-path*
  (asdf:system-relative-pathname :pgcharts "web/footer.html"))

(defvar *serve-from-cache* nil
  "Set to t to serve static resources from in-memory cache.")


;;;
;;; System integration: configuration file.
;;;
(defun expand-user-homedir-pathname (namestring)
  "Expand NAMESTRING replacing leading ~ with (user-homedir-pathname)"
  (typecase namestring
    (pathname namestring)
    (string
     (cond ((or (string= "~" namestring) (string= "~/" namestring))
            (user-homedir-pathname))

           ((and (<= 2 (length namestring))
                 (char= #\~ (aref namestring 0))
                 (char= #\/ (aref namestring 1)))
            (uiop:merge-pathnames*
             (uiop:parse-unix-namestring (subseq namestring 2))
             (user-homedir-pathname)))

           (t
            (uiop:parse-unix-namestring namestring))))))

(defun set-config-filename (namestring)
  (setf *config-filename* (expand-user-homedir-pathname namestring)))


;;;
;;; Defaults, organized in sections, with proper use facing option names
;;;
(defvar *sections-variables*
  '(("pgcharts"
     ("dburi"   *dburi*)
     ("port"    *listen-port*      parse-integer)
     ("address" *listen-address*   parse-listen-address)
     ("pidfile" *pidfile*          check-file-path)
     ("logfile" *logfile*          check-file-path))))


;;;
;;; Turn CL list of lists into INI files and back, and also take care of
;;; changing the dynamic variables values.
;;;
(defun read-config (&optional (filename *config-filename*))
  "Read the FILENAME INI file and set the special variables accordingly."
  (when (probe-file filename)
    (let* ((ini  (ini:make-config))
           (conf (ini:read-files ini (list filename))))
      (loop :for (section . options) :in *sections-variables*
         :do (loop :for (option var check-fun) :in options
                :when (ini:has-option-p conf section option)
                :do (let ((value (ini:get-option conf section option)))
                      (setf (symbol-value var)
                            (if check-fun
                                (handler-case
                                    (funcall check-fun value)
                                  ;; allow reading broken config
                                  (condition (c)
                                    (warn "Validation function ~s failed on ~s: ~a"
                                          check-fun
                                          value
                                          c)
                                    value))
                                ;; no check, just use value
                                value)))))
      conf)))

(defun write-current-config (stream)
  "Write the current configuration of pginstall in STREAM."
  (let ((config (ini:make-config)))
    (loop for (section . options) in *sections-variables*
       do (progn
            (ini:add-section config section)
            (loop for (option var check-fun) in options
               do (ini:set-option config section option (symbol-value var)))))
    (ini:write-stream config stream)
    config))

(defun save-config (&optional (pathname
                               (expand-user-homedir-pathname *config-filename*)))
  "Save the current configuration of pginstall in FILENAME."
  (with-open-file (s pathname
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format :utf8)
    (write-current-config s)))

(defun config-value (option-name)
  "Get the current value of the given OPTION-NAME."
  (loop :for (section . options) :in *sections-variables*
     :for value := (loop :for (option var check-fun) :in options
                      :when (string-equal option-name option)
                      :return (symbol-value var))
     :when value
     :return value))

(defun (setf config-value) (newvalue option-name)
  "Set configuration variable OPTION-NAME to NEWVALUE."
  (loop :for (section . options) :in *sections-variables*
     :do (loop for (option var check-fun) :in options
            :when (string-equal option-name option)
            :do (progn
                  (funcall check-fun newvalue)
                  (setf (symbol-value var) newvalue)))))


;;;
;;; pidfile reading
;;;
(defun read-pid (&optional (pidfile *pidfile*))
  "Read the server's pid from *pidfile* and return it as a string."
  (with-open-file (s pidfile) (read-line s)))

(defun kill-pid (pid &optional (sig "TERM"))
  "Send given SIG to Unix process PID."
  (multiple-value-bind (output error code)
      (uiop:run-program `("/bin/kill" ,(format nil "-~a" sig) ,pid)
                        :output :string
                        :error :string
                        :ignore-error-status t)
    (declare (ignore output error))
    (= 0 code)))


;;
;; Validation functions
;;
(defun check-and-make-directory (value)
  "Check that VALUE is a valid pathname and create a directory if it doesn't
   already exists."
  (ensure-directories-exist
   (uiop:ensure-directory-pathname (expand-user-homedir-pathname value))))

(defun check-executable (value)
  "Check that VALUE is the pathname of a valid executable file."
  value)

(defun check-file-path (path)
  "Check that we can open a file at given PATH."
  (let ((expanded-path (expand-user-homedir-pathname path)))
    (ensure-directories-exist (directory-namestring expanded-path))
    ;; then return expanded path
    expanded-path))

(defun check-log-setting (log-threshold &optional (default :notice))
  "Read a log threshold setting from CLI or INI file"
  (let ((threshold (find-symbol (string-upcase log-threshold) "KEYWORD")))
    (if (member threshold '(:panic :fatal :log :error :warning
                            :notice :info :debug :data))
        threshold
        (progn
          (warn "Didn't recognize log threshold ~s, using ~s instead."
                log-threshold (symbol-name default))
          default))))

(defun parse-listen-address (listen-address)
  "Change * into nil. Don't try to double-guess hunchentoot on what is a
   proper CNAME or IP address (ipv6 etc)."
  (if (or (string-equal "NIL" listen-address)
          (string= "*" listen-address))
      nil
      listen-address))
